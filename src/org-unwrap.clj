#!/usr/bin/env bb

;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns unwrap-text
  (:require [clojure.string :as str]
            [babashka.cli :as cli]))

(def cli-options
  {:input {:desc "Input file path"
           :alias :i}
   :output {:desc "Output file path (defaults to stdout if not provided)"
            :alias :o}
   :help {:desc "Show help"
          :alias :h}})

(defn show-help []
  (println "Unwrap Paragraphs and List Items in Text Files")
  (println)
  (println "Usage: unwrap-text.clj [options] [file]")
  (println "       unwrap-text.clj -i input.txt [-o output.txt]")
  (println "       unwrap-text.clj --input input.txt [--output output.txt]")
  (println)
  (println "Options:")
  (println "  -i, --input FILE    Input file path")
  (println "  -o, --output FILE   Output file path (defaults to stdout if not provided)")
  (println "  -h, --help          Show this help message")
  (println)
  (println "If a file is provided without options, it will be used as the input file")
  (println "and the result will be printed to stdout."))

(defn list-item? [line]
  (boolean (re-matches #"^\s*[-+*]\s+.*" line)))

(defn org-headline? [line]
  (boolean (re-matches #"^\s*\*+\s+.*" line)))

(defn metadata-line? [line]
  (boolean (re-matches #"^\s*#\+.*" line)))

(defn property-line? [line]
  (or 
   (boolean (re-matches #"^\s*:.*:$" line))
   (boolean (re-matches #"^\s*:.*:.*:$" line))))

(defn empty-line? [line]
  (str/blank? line))

;; This function is no longer needed with the revised algorithm

(defn- should-append? [current-line next-line]
  (cond
    ;; Don't append if current line is empty
    (empty-line? current-line) false
    
    ;; Don't append if next line is empty
    (empty-line? next-line) false
    
    ;; Don't append to headlines
    (org-headline? current-line) false
    
    ;; Don't append to metadata
    (metadata-line? current-line) false
    
    ;; Don't append to property drawers
    (property-line? current-line) false
    
    ;; Don't append to a list item if the next line is a new list item
    (and (list-item? current-line) (list-item? next-line)) false
    
    ;; Don't append if next line is a headline
    (org-headline? next-line) false
    
    ;; Don't append if next line is metadata
    (metadata-line? next-line) false
    
    ;; Don't append if next line is a property drawer
    (property-line? next-line) false
    
    ;; The default case: append
    :else true))

(defn unwrap-text [input]
  (let [lines (str/split-lines input)
        unwrapped
        (loop [result    []
               remaining lines]
          (if (empty? remaining)
            result
            (let [current    (first remaining)
                  rest-lines (rest remaining)
                  next-line  (first rest-lines)]
              (cond
                ;; Keep empty lines intact
                (empty-line? current)
                (recur (conj result current) rest-lines)
                
                ;; If next line doesn't exist or we shouldn't append
                (or (nil? next-line) (not (should-append? current next-line)))
                (recur (conj result current) rest-lines)
                
                ;; Append the next line to the current one
                :else
                (let [ ;; Normalize whitespace when joining lines
                      trimmed-next    (str/trim next-line)
                      ;; When joining list items, normalize internal whitespace too
                      normalized-next (if (list-item? current)
                                        (str/replace trimmed-next #"\s+" " ")
                                        trimmed-next)
                      new-current     (str current " " normalized-next)]
                  (recur result (cons new-current (rest rest-lines))))))))]
    (str/join "\n" unwrapped)))

(defn process-file [opts]
  (if (nil? (:input opts))
    (show-help)
    (let [input-path (:input opts)
          output-path (:output opts)
          input-content (try
                          (slurp input-path)
                          (catch Exception e
                            (println "Error: Could not read file" input-path ":" (.getMessage e))
                            (System/exit 1)))
          result (unwrap-text input-content)]
      (if output-path
        (do
          (spit output-path result)
          (println "Processed file saved to:" output-path))
        ;; Print to stdout if no output file is specified
        (println result)))))

(defn -main [& args]
  (if (empty? args)
    (show-help)
    (let [first-arg (first args)
          is-help (or (= first-arg "-h") (= first-arg "--help"))
          parsed-opts (cli/parse-opts args {:spec cli-options})
          ;; Handle the case where the first arg is a file without a flag
          final-opts (if (and (not is-help)
                             (not (str/starts-with? first-arg "-"))
                             (not (:input parsed-opts)))
                       (assoc parsed-opts :input first-arg)
                       parsed-opts)]
      (cond
        is-help (show-help)
        (:help final-opts) (show-help)
        :else (process-file final-opts)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
