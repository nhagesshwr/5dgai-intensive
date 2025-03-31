(import [kaggle_api [init-api get-competitions]])

(let [api (init-api)]
  (print (get-competitions api 
                          :search "gen ai" 
                          :category "featured" 
                          :sort-by "latestDeadline")))

(import [kaggle_api [init-api get-competition-files]])

(let [api (init-api)]
  (print (get-competition-files api competition)))

(import [kaggle_api [init-api get-datasets]])

(let [api (init-api)]
  (print (get-datasets api 
                      :search "generative ai" 
                      :sort-by "votes")))

(import [kaggle_api [init-api get-kernels]])

(let [api (init-api)]
  (print (get-kernels api 
                     :search "generative ai" 
                     :language "python")))
