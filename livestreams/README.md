# 5-Day Gen AI Intensive Course Livestream Transcriptions

This directory contains transcriptions and summaries of the 5-Day Gen AI Intensive Course livestreams from Kaggle.

## Livestream Schedule

- **Day 1**: March 31, 2025 at 2:00 PM - Introduction to Generative AI
- **Day 2**: April 1, 2025 at 2:00 PM - Prompt Engineering
- **Day 3**: April 2, 2025 at 2:00 PM - Building with the API
- **Day 4**: April 3, 2025 at 2:00 PM - Multimodal Applications
- **Day 5**: April 4, 2025 at 2:00 PM - Production Deployment

## File Structure

Each day's livestream produces the following files:

- `day{N}-{timestamp}.audio.mp3` - The audio recording of the livestream
- `day{N}-{timestamp}.transcript.txt` - The timestamped transcript
- `day{N}-{timestamp}.summary.txt` - A summary generated with Gemini API

## Manual Capture

You can manually capture any livestream using the `capture-livestreams.sh` script:

```bash
# Process Day 1 (already streamed)
./scripts/capture-livestreams.sh 1

# Capture upcoming livestreams
./scripts/capture-livestreams.sh 2  # For Day 2
```

## Automated Capture

The `setup-livestream-cron.sh` script sets up cron jobs to automatically capture all livestreams:

```bash
# Set up cron jobs for all days
./scripts/setup-livestream-cron.sh
```

## Dependencies

The capture system requires:

- yt-dlp for downloading YouTube content
- whisper.cpp for transcription
- Google's Gemini API for summarization

## Livestream URLs

- Day 1: https://www.youtube.com/watch?v=WpIfAeCIFc0
- Day 2: https://www.youtube.com/watch?v=AjpjCHdIINU
- Day 3: https://www.youtube.com/watch?v=g6MVIEzFTjY
- Day 4: https://www.youtube.com/watch?v=AN2tpHi26OE
- Day 5: https://www.youtube.com/watch?v=eZ-8UQ_t4YM

## Playlist

The complete playlist is available at: https://www.youtube.com/playlist?list=PLqFaTIg4myu-lbBTrUpoQQIzZZxvrOaP5