#!/usr/bin/env hy
;; Livestream Transcription and Summarization
;; Listens to YouTube livestreams, transcribes them, and generates summaries

(import subprocess
        os
        argparse
        signal
        time
        sys
        threading
        [google.genai [genai types]])

(defn setup-argparse []
  "Configure command-line argument parsing"
  (setv parser (argparse.ArgumentParser :description "Listen to YouTube live stream using whisper.cpp and summarize with Gemini"))
  (.add_argument parser "url" :help "YouTube live stream URL")
  (.add_argument parser "--model" :default "small" :help "Whisper model to use (tiny, base, small, medium, large)")
  (.add_argument parser "--lang" :default "en" :help "Language code for transcription")
  (.add_argument parser "--segment-length" :type int :default 10 :help "Length of audio segments in seconds")
  (.add_argument parser "--summary-interval" :type int :default 300 :help "Generate summaries every N seconds")
  (.add_argument parser "--output-dir" :default "transcripts" :help "Directory to save transcripts and summaries")
  (.parse_args parser))

(defn setup-fifo []
  "Create named pipe for audio streaming"
  (setv fifo-path "/tmp/whisper_stream.wav")
  (when (os.path.exists fifo-path)
    (os.remove fifo-path))
  (os.mkfifo fifo-path)
  fifo-path)

(defn setup-output-dir [args]
  "Create output directory for transcripts and summaries"
  (when (not (os.path.exists args.output_dir))
    (os.makedirs args.output_dir))
  (setv timestamp (time.strftime "%Y%m%d-%H%M%S"))
  (setv output-base f"{args.output_dir}/{timestamp}")
  (setv transcript-path f"{output-base}.transcript.txt")
  (setv summary-path f"{output-base}.summary.txt")
  
  ;; Create empty files
  (with [f (open transcript-path "w")] None)
  (with [f (open summary-path "w")] None)
  
  {"transcript_path" transcript-path
   "summary_path" summary-path
   "base_path" output-base})

(defn signal-handler [sig frame terminated processes]
  "Handle termination signals"
  (print "\nReceived signal to terminate. Cleaning up...")
  (setv (get terminated 0) True)
  (for [process processes]
    (try
      (.terminate process)
      (except [e Exception]
        (print f"Error terminating process: {e}"))))
  (when (os.path.exists "/tmp/whisper_stream.wav")
    (os.remove "/tmp/whisper_stream.wav"))
  (print "Cleanup complete. Exiting.")
  (sys.exit 0))

(defn start-stream-listener [url fifo-path segment-length]
  "Start ffmpeg process to listen to the stream"
  (setv cmd [
    "ffmpeg"
    "-re"
    "-i" url
    "-f" "segment"
    "-segment_time" (str segment-length)
    "-c:a" "pcm_s16le"
    "-ar" "16000"
    "-ac" "1"
    "-f" "wav"
    fifo-path
  ])
  
  (print "Starting stream listener...")
  (print f"Command: {' '.join cmd}")
  
  (subprocess.Popen cmd
                    :stdout subprocess.PIPE
                    :stderr subprocess.PIPE))

(defn start-transcriber [model lang fifo-path]
  "Start whisper.cpp process to transcribe audio"
  (setv cmd [
    "whisper"
    "-m" f"models/ggml-{model}.bin"
    "--language" lang
    "-f" fifo-path
    "--output-txt"
  ])
  
  (print "Starting transcriber...")
  (print f"Command: {' '.join cmd}")
  
  (subprocess.Popen cmd
                    :stdout subprocess.PIPE
                    :stderr subprocess.STDOUT
                    :text True))

(defn process-transcription [transcriber transcript-path terminated]
  "Process transcription output and save to file"
  (print "Processing transcriptions...")
  (with [transcript-file (open transcript-path "a")]
    (while (not (get terminated 0))
      (setv line (.readline transcriber.stdout))
      (when (not line)
        (break))
      (setv line (.strip line))
      (when (and line (not (.startswith line "[")))
        (print f"Transcript: {line}")
        (.write transcript-file f"{line}\n")
        (.flush transcript-file))
      (time.sleep 0.1))))

(defn summarize-with-gemini [transcript summary-path]
  "Generate summary of transcript with Gemini API"
  (print "Generating summary with Gemini...")
  
  ;; Initialize the API client
  (setv client (genai.Client))
  
  ;; Read the transcript
  (with [f (open transcript "r")]
    (setv transcript-text (.read f)))
  
  ;; Skip if transcript is too short
  (when (< (len transcript-text) 50)
    (print "Transcript too short for summarization")
    (return))
  
  ;; Create prompt for summarization
  (setv prompt (+ 
    "Summarize the following livestream transcript in 3-5 bullet points. "
    "Focus on the main topics discussed and key insights. "
    "Format as bullet points with timestamps if available:\n\n"
    transcript-text))
  
  ;; Generate summary
  (try
    (setv response (.generate_content client.models
                                      :model "gemini-1.5-flash"
                                      :contents [prompt]))
    
    ;; Save summary
    (with [f (open summary-path "w")]
      (.write f "--- Livestream Summary ---\n\n")
      (.write f (.text response))
      (.write f "\n\n--- Generated at " (time.strftime "%Y-%m-%d %H:%M:%S") " ---\n"))
    
    (print "Summary generated and saved to" summary-path)
    (except [e Exception]
      (print f"Error generating summary: {e}"))))

(defn periodic-summarization [transcript-path summary-path interval terminated]
  "Periodically summarize the transcript"
  (print f"Starting periodic summarization every {interval} seconds...")
  (while (not (get terminated 0))
    (time.sleep interval)
    (when (not (get terminated 0))
      (print "Generating periodic summary...")
      (summarize-with-gemini transcript-path summary-path))))

(defn main []
  "Main function to run the livestream transcription and summarization"
  ;; Parse command-line arguments
  (setv args (setup-argparse))
  
  ;; Set up output paths
  (setv output-paths (setup-output-dir args))
  (setv transcript-path (get output-paths "transcript_path"))
  (setv summary-path (get output-paths "summary_path"))
  
  ;; Create named pipe
  (setv fifo-path (setup-fifo))
  
  ;; Initialize termination flag (using list for mutability)
  (setv terminated [False])
  
  ;; Start processes
  (setv stream-listener (start-stream-listener args.url fifo-path args.segment_length))
  (setv transcriber (start-transcriber args.model args.lang fifo-path))
  
  ;; Set up signal handler
  (signal.signal signal.SIGINT 
                (fn [sig frame] 
                  (signal-handler sig frame terminated [stream-listener transcriber])))
  
  ;; Start processing threads
  (setv transcription-thread (threading.Thread :target process-transcription
                                              :args [transcriber transcript-path terminated]))
  (setv summary-thread (threading.Thread :target periodic-summarization
                                        :args [transcript-path summary-path args.summary_interval terminated]))
  
  (.start transcription-thread)
  (.start summary-thread)
  
  ;; Wait for threads to complete
  (.join transcription-thread)
  (.join summary-thread)
  
  ;; Cleanup
  (.terminate stream-listener)
  (.terminate transcriber)
  (when (os.path.exists fifo-path)
    (os.remove fifo-path))
  
  (print "Transcription and summarization complete."))

(when (= __name__ "__main__")
  (main))