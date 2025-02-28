#!/usr/bin/env bb

;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[cheshire.core :as json]
         '[clojure.tools.cli :as cli]
         '[clojure.pprint :as pprint]
         '[clj-yaml.core :as yaml])

(def cli-options
  [["-m" "--max-level LEVEL" "Show headlines with level <= LEVEL"
    :parse-fn #(Integer/parseInt %)
    :validate [#(> % 0) "Must be a positive number"]]
   ["-n" "--min-level LEVEL" "Show headlines with level >= LEVEL"
    :parse-fn #(Integer/parseInt %)
    :validate [#(> % 0) "Must be a positive number"]]
   ["-c" "--custom-id REGEX" "Show headlines with CUSTOM_ID property matching regex"
    :parse-fn re-pattern]
   ["-C" "--section-custom-id REGEX" "Show headlines within sections whose CUSTOM_ID property matches regex"
    :parse-fn re-pattern]
   ["-T" "--title REGEX" "Show headlines whose title matches regex"
    :parse-fn re-pattern]
   ["-t" "--section-title REGEX" "Show headlines within sections whose title matches regex"
    :parse-fn re-pattern]
   ["-H" "--html" "Convert content to HTML"]
   ["-M" "--markdown" "Convert content to Markdown"]
   ["-f" "--format FORMAT" "Output format: json, edn, yaml, or org"
    :default "json"
    :validate [#(contains? #{"json" "edn" "yaml" "org"} %) "Must be one of: json, edn, yaml, org"]]
   ["-h" "--help" "Show this help"]])

;; Utility functions
(defn is-comment? [line]
  (boolean (re-matches #"^#.*$" line)))

(defn usage [options-summary]
  (str/join \newline
            ["Usage: org-parse [options] org-file"
             ""
             "Options:"
             options-summary
             ""
             "Examples:"
             "  org-parse notes.org                       # Process all headlines"
             "  org-parse -m 2 notes.org                  # Process headlines with level <= 2"
             "  org-parse -n 2 notes.org                  # Process headlines with level >= 2"
             "  org-parse -m 3 -n 2 notes.org             # Process headlines with 2 <= level <= 3"
             "  org-parse -c \"section[0-9]+\" notes.org    # Headlines with CUSTOM_ID matching regex"
             "  org-parse -m 2 -c \"^ch\" notes.org         # Combine level and CUSTOM_ID filters"
             "  org-parse -t \"Projects\" notes.org         # Headlines within sections titled 'Projects'"
             "  org-parse -t \"^(Tasks|Projects)$\" notes.org  # Headlines within 'Tasks' or 'Projects'"
             "  org-parse -T \"TODO\" notes.org             # Headlines with title matching 'TODO'"
             "  org-parse -C \"chapter\\d+\" notes.org      # Headlines within sections with CUSTOM_ID matching regex"
             "  org-parse -H notes.org                   # Convert content to HTML"
             "  org-parse -M notes.org                   # Convert content to Markdown"
             "  org-parse -f edn notes.org               # Output in EDN format"
             "  org-parse -f yaml notes.org              # Output in YAML format"
             "  org-parse -f org notes.org               # Output in Org format (default if no conversion)"]))

;; HTML conversion functions
(defn extract-links [text]
  (let [link-pattern #"\[\[([^\]]+)\]\[([^\]]+)\]\]|\[\[([^\]]+)\]\]"
        matches      (re-seq link-pattern text)
        links        (map (fn [match]
                            (if (nth match 2) ; Full link with description
                              {:full (first match)
                               :url  (second match)
                               :text (nth match 2)}
                              {:full (first match)
                               :url  (nth match 3)
                               :text (nth match 3)}))
                          matches)]
    links))

(defn replace-links-with-placeholders [text]
  (let [links           (extract-links text)
        placeholder-map (zipmap
                         (map :full links)
                         (map-indexed (fn [idx _] (str "LINKPLACEHOLDER" idx)) links))]
    [(reduce (fn [t [original placeholder]]
               (str/replace t original placeholder))
             text
             placeholder-map)
     placeholder-map
     links]))

(defn restore-links-as-html [text placeholder-map links]
  (reduce (fn [t [idx placeholder]]
            (let [link      (nth links idx)
                  html-link (str "<a href=\"" (:url link) "\">" (:text link) "</a>")]
              (str/replace t placeholder html-link)))
          text
          (map-indexed (fn [idx placeholder] [idx placeholder])
                       (map second placeholder-map))))

(defn org-to-html-markup [text]
  (let [[text-with-placeholders placeholder-map links]
        (replace-links-with-placeholders text)
        converted (-> text-with-placeholders
                      (str/replace #"\*([^\*]+)\*" "<strong>$1</strong>")      ;; bold
                      (str/replace #"/([^/]+)/" "<em>$1</em>")                 ;; italic
                      (str/replace #"_([^_]+)_" "<u>$1</u>")                   ;; underline
                      (str/replace #"\+([^\+]+)\+" "<del>$1</del>")            ;; strikethrough
                      (str/replace #"~([^~]+)~" "<code>$1</code>")             ;; code
                      (str/replace #"=([^=]+)=" "<code>$1</code>"))]           ;; verbatim
    (restore-links-as-html converted placeholder-map links)))

(defn is-unordered-list-item? [line]
  (boolean (re-matches #"^\s*[-+*]\s+.*$" line)))

(defn is-ordered-list-item? [line]
  (boolean (re-matches #"^\s*\d+[.)]\s+.*$" line)))

(defn is-list-item? [line]
  (or (is-unordered-list-item? line) (is-ordered-list-item? line)))

(defn clean-list-item [line]
  (cond
    (is-unordered-list-item? line)
    (str/replace line #"^\s*[-+*]\s+" "")

    (is-ordered-list-item? line)
    (str/replace line #"^\s*\d+[.)]\s+" "")

    :else line))

(defn process-list-items [lines]
  (if (empty? lines)
    lines
    (let [first-line (first lines)]
      (if (is-unordered-list-item? first-line)
        (str "<ul>\n"
             (str/join "\n" (map #(str "<li>" (org-to-html-markup (clean-list-item %)) "</li>")
                                 (take-while is-list-item? lines)))
             "\n</ul>")
        (str "<ol>\n"
             (str/join "\n" (map #(str "<li>" (org-to-html-markup (clean-list-item %)) "</li>")
                                 (take-while is-list-item? lines)))
             "\n</ol>")))))

(defn content-to-html [content-lines]
  (if (empty? content-lines)
    ""
    (loop [remaining-lines content-lines
           result          []]
      (if (empty? remaining-lines)
        (str/join "\n" result)
        (let [current-line (first remaining-lines)]
          (cond
            ;; List processing
            (is-list-item? current-line)
            (let [list-items (take-while is-list-item? remaining-lines)
                  list-html  (process-list-items list-items)]
              (recur (drop (count list-items) remaining-lines)
                     (conj result list-html)))

            ;; Regular paragraph text
            :else
            (recur (rest remaining-lines)
                   (conj result (str "<p>" (org-to-html-markup current-line) "</p>")))))))))

;; Markdown conversion functions
(defn extract-links-md [text]
  (let [link-pattern #"\[\[([^\]]+)\]\[([^\]]+)\]\]|\[\[([^\]]+)\]\]"
        matches      (re-seq link-pattern text)
        links        (map (fn [match]
                            (if (nth match 2) ; Full link with description
                              {:full (first match)
                               :url  (second match)
                               :text (nth match 2)}
                              {:full (first match)
                               :url  (nth match 3)
                               :text (nth match 3)}))
                          matches)]
    links))

(defn replace-links-with-placeholders-md [text]
  (let [links           (extract-links-md text)
        placeholder-map (zipmap
                         (map :full links)
                         (map-indexed (fn [idx _] (str "LINKPLACEHOLDER" idx)) links))]
    [(reduce (fn [t [original placeholder]]
               (str/replace t original placeholder))
             text
             placeholder-map)
     placeholder-map
     links]))

(defn restore-links-as-markdown [text placeholder-map links]
  (reduce (fn [t [idx placeholder]]
            (let [link    (nth links idx)
                  md-link (str "[" (:text link) "](" (:url link) ")")]
              (str/replace t placeholder md-link)))
          text
          (map-indexed (fn [idx placeholder] [idx placeholder])
                       (map second placeholder-map))))

(defn org-to-markdown-markup [text]
  (let [[text-with-placeholders placeholder-map links]
        (replace-links-with-placeholders-md text)
        converted (-> text-with-placeholders
                      (str/replace #"\*([^\*]+)\*" "**$1**")      ;; bold
                      (str/replace #"/([^/]+)/" "*$1*")           ;; italic
                      (str/replace #"_([^_]+)_" "_$1_")           ;; underline (keep as is in Markdown)
                      (str/replace #"\+([^\+]+)\+" "~~$1~~")      ;; strikethrough
                      (str/replace #"~([^~]+)~" "`$1`")           ;; code
                      (str/replace #"=([^=]+)=" "`$1`"))]         ;; verbatim
    (restore-links-as-markdown converted placeholder-map links)))

(defn is-unordered-list-item-md? [line]
  (boolean (re-matches #"^\s*[-+*]\s+.*$" line)))

(defn is-ordered-list-item-md? [line]
  (boolean (re-matches #"^\s*\d+[.)]\s+.*$" line)))

(defn is-list-item-md? [line]
  (or (is-unordered-list-item-md? line) (is-ordered-list-item-md? line)))

(defn clean-list-item-md [line]
  (cond
    (is-unordered-list-item-md? line)
    (let [spaces (count (re-find #"^\s*" line))]
      (str (apply str (repeat spaces " ")) "- "
           (str/trim (str/replace line #"^\s*[-+*]\s+" ""))))

    (is-ordered-list-item-md? line)
    (let [spaces (count (re-find #"^\s*" line))
          num    (re-find #"\d+" line)]
      (str (apply str (repeat spaces " ")) num ". "
           (str/trim (str/replace line #"^\s*\d+[.)]\s+" ""))))

    :else line))

(defn process-list-items-md [lines]
  (if (empty? lines)
    lines
    (mapv (fn [line]
            (let [cleaned (clean-list-item-md line)]
              (org-to-markdown-markup cleaned)))
          lines)))

(defn content-to-markdown [content-lines]
  (if (empty? content-lines)
    ""
    (let [processed-lines (mapv org-to-markdown-markup content-lines)
          ;; Join lines preserving line breaks for Markdown
          result          (str/join "\n" processed-lines)]
      result)))

;; Org-mode parsing functions
(defn property-line? [line]
  (and (boolean (re-matches #"^\s*:[\w_-]+:\s*.*$" line))
       (not (str/includes? (str/trim line) ":PROPERTIES:"))
       (not (str/includes? (str/trim line) ":END:"))))

(defn parse-property [line]
  (when-let [[_ key value] (re-matches #"^\s*:([\w_-]+):\s*(.*)$" line)]
    [(keyword (str/lower-case key)) (str/trim value)]))

(defn in-property-drawer? [lines]
  (and (seq lines)
       (str/starts-with? (str/trim (first lines)) ":PROPERTIES:")))

(defn process-property-drawer [lines]
  (loop [remaining-lines lines
         properties      {}
         processed-count 0]
    (let [current-line (first remaining-lines)]
      (cond
        ;; End of lines or end of drawer
        (or (empty? remaining-lines)
            (str/includes? (str/trim current-line) ":END:"))
        [properties (inc processed-count)]

        ;; Property line
        (property-line? current-line)
        (if-let [[key value] (parse-property current-line)]
          (recur (rest remaining-lines)
                 (assoc properties key value)
                 (inc processed-count))
          (recur (rest remaining-lines)
                 properties
                 (inc processed-count)))

        ;; Any other line inside drawer - skip it
        :else
        (recur (rest remaining-lines)
               properties
               (inc processed-count))))))

;; Path tracking needs to be updated for HTML/Markdown-converted titles
(defn parse-org-file [file-path convert-to-html? convert-to-markdown?]
  (with-open [rdr (io/reader file-path)]
    (loop [lines            (line-seq rdr)
           current-headline nil
           headlines        []
           path-stack       []]
      (if-let [line (first lines)]
        (cond
          ;; New headline
          (str/starts-with? line "*")
          (let [new-level      (count (re-find #"^\*+" line))
                orig-title     (str/trim (subs line new-level))
                title          (cond
                                 convert-to-html?     (org-to-html-markup orig-title)
                                 convert-to-markdown? (org-to-markdown-markup orig-title)
                                 :else                orig-title)
                ;; Store original title in path but converted title in headline
                path-title     (cond
                                 (or convert-to-html? convert-to-markdown?) orig-title
                                 :else                                      title)
                ;; Update path stack based on new headline level
                new-path-stack (if (empty? path-stack)
                                 [path-title]
                                 (let [current-level (count path-stack)]
                                   (cond
                                     (> new-level current-level)
                                     (conj path-stack path-title)
                                     (= new-level current-level)
                                     (conj (vec (butlast path-stack)) path-title)
                                     :else (conj (vec (take (dec new-level) path-stack)) path-title))))
                new-headline   {:level      new-level
                                :title      title
                                :content    []
                                :properties {}
                                :path       new-path-stack}]
            (recur (rest lines)
                   new-headline
                   (if current-headline
                     (conj headlines (update current-headline :content
                                             #(filterv (fn [line]
                                                         (and (not (str/blank? line))
                                                              (not (is-comment? line)))) %)))
                     headlines)
                   new-path-stack))

          ;; Property drawer
          (and current-headline (in-property-drawer? lines))
          (let [[props lines-consumed] (process-property-drawer lines)
                updated-headline       (update current-headline :properties merge props)]
            (recur (drop lines-consumed lines)
                   updated-headline
                   headlines
                   path-stack))

          ;; Regular content
          :else
          (recur (rest lines)
                 (if current-headline
                   (update current-headline :content conj (str/trim line))
                   current-headline)
                 headlines
                 path-stack))

        ;; End of file
        (if current-headline
          (conj headlines (update current-headline :content
                                  #(filterv (fn [line]
                                              (and (not (str/blank? line))
                                                   (not (is-comment? line)))) %)))
          headlines)))))

;; Filtering functions
(defn filter-headlines-by-level [headlines min-level max-level]
  (filter #(and (or (nil? min-level) (>= (:level %) min-level))
                (or (nil? max-level) (<= (:level %) max-level)))
          headlines))

(defn filter-headlines-by-headline [headlines headline-pattern]
  (if headline-pattern
    (filter (fn [headline]
              (re-find headline-pattern (:title headline)))
            headlines)
    headlines))

(defn filter-headlines-by-custom-id [headlines custom-id-pattern]
  (if custom-id-pattern
    (filter (fn [headline]
              (when-let [custom-id (get-in headline [:properties :custom_id])]
                (re-find custom-id-pattern custom-id)))
            headlines)
    headlines))

(defn filter-headlines-by-section [headlines section-pattern]
  (if section-pattern
    (filter (fn [headline]
              (some (fn [path-item]
                      (re-find section-pattern path-item))
                    (:path headline)))
            headlines)
    headlines))

(defn filter-headlines-by-section-custom-id [headlines section-custom-id-pattern]
  (if section-custom-id-pattern
    (let [matching-custom-ids (atom #{})]
      ;; First, identify all custom IDs that match the pattern
      (doseq [headline headlines]
        (when-let [custom-id (get-in headline [:properties :custom_id])]
          (when (re-find section-custom-id-pattern custom-id)
            (swap! matching-custom-ids conj custom-id))))

      ;; Then filter headlines that are in a section with a matching custom ID
      (filter (fn [headline]
                (let [path-custom-ids (for [path-title (:path headline)]
                                        ;; Find the custom-id for this section by title
                                        ;; This is a simplification - in a real implementation
                                        ;; you would need a more robust way to identify sections
                                        (some (fn [h]
                                                (when (= path-title (:title h))
                                                  (get-in h [:properties :custom_id])))
                                              headlines))]
                  (some @matching-custom-ids (remove nil? path-custom-ids))))
              headlines))
    headlines))

;; Define print-org-structure before it's used in write-org
(defn print-org-structure [headlines]
  (doseq [headline headlines]
    ;; Print headline with proper number of asterisks
    (println (str (apply str (repeat (:level headline) "*"))
                  " " (:title headline)))

    ;; Print properties if any
    (when (seq (:properties headline))
      (println "  :PROPERTIES:")
      (doseq [[k v] (:properties headline)]
        (println (str "  :" (str/upper-case (name k)) ": " v)))
      (println "  :END:"))

    ;; Print content
    (when (seq (:content headline))
      (if (string? (:content headline))
        ;; If content has been converted to HTML/Markdown, it's a string
        (println (:content headline))
        ;; If original format, it's still a vector of lines
        (do
          (println) ; Add empty line after properties or headline
          (doseq [line (:content headline)]
            (println line))
          (println))))))

;; Output functions
(defn prepare-for-output [headlines format]
  (case format
    "edn"  (mapv #(update % :path (fn [path] (apply list path))) headlines)
    "json" headlines
    "yaml" headlines
    "org"  headlines))

(defn write-json [data file-path]
  (with-open [writer (io/writer file-path)]
    (json/generate-stream data writer {:pretty true})))

(defn write-edn [data file-path]
  (with-open [writer (io/writer file-path)]
    (binding [*print-length* nil
              *print-level*  nil
              *print-dup*    false]
      (pprint/pprint data writer))))

(defn write-yaml [data file-path]
  (with-open [writer (io/writer file-path)]
    (.write writer (yaml/generate-string data :dumper-options {:flow-style :block}))))

(defn write-org [headlines file-path]
  (with-open [writer (io/writer file-path)]
    (.write writer (with-out-str (print-org-structure headlines)))))

(defn clean-headline [headline convert-to-html? convert-to-markdown?]
  (let [cleaned (-> headline
                    (update :content #(filterv (fn [line]
                                                 (and (not (str/blank? line))
                                                      (not (is-comment? line)))) %))
                    (update :properties #(into {} (remove (fn [[_ v]] (str/blank? v)) %))))]
    (cond
      convert-to-html?
      (-> cleaned
          (update :content content-to-html)
          (update :title org-to-html-markup))

      convert-to-markdown?
      (-> cleaned
          (update :content content-to-markdown)
          (update :title org-to-markdown-markup))

      :else
      cleaned)))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (when (and (:html options) (:markdown options))
      (throw (Exception. "Error: Both HTML and Markdown conversion requested")))
    (cond
      (:help options)    (println (usage summary))
      errors             (do (println (str/join \newline errors))
                             (println (usage summary)))
      (empty? arguments) (println (usage summary))
      :else
      (let [file-path                 (first arguments)
            min-level                 (:min-level options)
            max-level                 (:max-level options)
            custom-id-pattern         (:custom-id options)
            section-title-pattern     (:section-title options)
            title-pattern             (:title options)
            section-custom-id-pattern (:section-custom-id options)
            convert-to-html?          (:html options)
            convert-to-markdown?      (:markdown options)
            output-format             (:format options)
            all-headlines             (parse-org-file file-path convert-to-html? convert-to-markdown?)
            level-filtered            (filter-headlines-by-level
                                       all-headlines min-level max-level)
            title-filtered            (filter-headlines-by-headline
                                       level-filtered title-pattern)
            custom-id-filtered        (filter-headlines-by-custom-id
                                       title-filtered custom-id-pattern)
            section-filtered          (filter-headlines-by-section
                                       custom-id-filtered section-title-pattern)
            filtered-headlines        (filter-headlines-by-section-custom-id
                                       section-filtered section-custom-id-pattern)
            clean-headlines           (mapv #(clean-headline
                                              % convert-to-html? convert-to-markdown?) filtered-headlines)
            output-path               (str/replace file-path #"\.org$" (str "." output-format))]

        ;; Output handling based on format and conversion options
        (cond
          ;; When specific output format is requested, write to file
          (not= output-format "json") ; If it's not the default
          (let [prepared-headlines (prepare-for-output clean-headlines output-format)]
            (case output-format
              "json" (write-json prepared-headlines output-path)
              "edn"  (write-edn prepared-headlines output-path)
              "yaml" (write-yaml prepared-headlines output-path)
              "org"  (write-org clean-headlines output-path))
            (println (str (str/upper-case output-format) " output written to " output-path)))

          ;; If HTML or Markdown conversion requested, use JSON as default
          (or convert-to-html? convert-to-markdown?)
          (let [prepared-headlines (prepare-for-output clean-headlines "json")]
            (write-json prepared-headlines output-path)
            (println (str "JSON output written to " output-path)))

          ;; Default behavior: print org structure to stdout if no other format specified
          :else
          (print-org-structure clean-headlines))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
