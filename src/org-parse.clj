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
   ["-t" "--title REGEX" "Show headlines whose title matches regex"
    :parse-fn re-pattern]
   ["-T" "--section-title REGEX" "Show headlines within sections whose title matches regex"
    :parse-fn re-pattern]
   ["-H" "--html" "Convert content to HTML"]
   ["-M" "--markdown" "Convert content to Markdown"]
   ["-l" "--include-level" "Include headlings level"]
   ["-f" "--format FORMAT" "Output format: json, edn, or yaml"
    :default "json"
    :validate [#(contains? #{"json" "edn" "yaml"} %) "Must be one of: json, edn, yaml"]]
   ["-h" "--help" "Show this help"]])

(defn usage [options-summary]
  (str/join
   \newline
   ["Usage: org-parse [options] org-file"
    ""
    "Options:"
    options-summary
    ""
    "Examples:"
    "  org-parse notes.org                         # Process all headlines"
    "  org-parse -m 2 notes.org                    # Process headlines with level <= 2"
    "  org-parse -n 2 notes.org                    # Process headlines with level >= 2"
    "  org-parse -m 3 -n 2 notes.org               # Process headlines with 2 <= level <= 3"
    "  org-parse -c \"section[0-9]+\" notes.org    # Headlines with CUSTOM_ID matching regex"
    "  org-parse -m 2 -c \"^ch\" notes.org         # Combine level and CUSTOM_ID filters"
    "  org-parse -T \"Projects\" notes.org         # Headlines within sections titled 'Projects'"
    "  org-parse -T \"^(WAIT|DONE)$\" notes.org    # Headlines within 'WAIT' or 'DONE'"
    "  org-parse -t \"TODO\" notes.org             # Headlines with title matching 'TODO'"
    "  org-parse -C \"chapter\\d+\" notes.org      # Headlines within sections with CUSTOM_ID matching regex"
    "  org-parse -H notes.org                      # Convert content to HTML"
    "  org-parse -M notes.org                      # Convert content to Markdown"
    "  org-parse -f edn notes.org                  # Output in EDN format"
    "  org-parse -f yaml notes.org                 # Output in YAML format"]))

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
  (reduce
   (fn [t [idx placeholder]]
     (let [link      (nth links idx)
           html-link (str "<a href=\"" (:url link) "\">" (:text link) "</a>")]
       (str/replace t placeholder html-link)))
   text
   (map-indexed (fn [idx placeholder] [idx placeholder])
                (map second placeholder-map))))

(defn restore-links-as-markdown [text placeholder-map links]
  (reduce
   (fn [t [idx placeholder]]
     (let [link    (nth links idx)
           md-link (str "[" (:text link) "](" (:url link) ")")]
       (str/replace t placeholder md-link)))
   text
   (map-indexed (fn [idx placeholder] [idx placeholder])
                (map second placeholder-map))))

(defn org-to-html [text]
  (let [[text-with-placeholders placeholder-map links]
        (replace-links-with-placeholders text)
        converted (-> text-with-placeholders
                      (str/replace #"\*([^\*]+)\*" "<strong>$1</strong>") ;; bold
                      (str/replace #"/([^/]+)/" "<em>$1</em>") ;; italic
                      (str/replace #"_([^_]+)_" "<u>$1</u>") ;; underline
                      (str/replace #"\+([^\+]+)\+" "<del>$1</del>") ;; strikethrough
                      (str/replace #"~([^~]+)~" "<code>$1</code>") ;; code
                      (str/replace #"=([^=]+)=" "<code>$1</code>"))] ;; verbatim
    (restore-links-as-html converted placeholder-map links)))

(defn org-to-markdown [text]
  (let [[text-with-placeholders placeholder-map links]
        (replace-links-with-placeholders text)
        converted (-> text-with-placeholders
                      (str/replace #"\*([^\*]+)\*" "**$1**") ;; bold
                      (str/replace #"/([^/]+)/" "*$1*")      ;; italic
                      (str/replace #"_([^_]+)_" "_$1_") ;; underline (keep as is in Markdown)
                      (str/replace #"\+([^\+]+)\+" "~~$1~~") ;; strikethrough
                      (str/replace #"~([^~]+)~" "`$1`")      ;; code
                      (str/replace #"=([^=]+)=" "`$1`"))] ;; verbatim
    (restore-links-as-markdown converted placeholder-map links)))

(defn is-comment? [line]
  (boolean (re-matches #"^#.*$" line)))

(defn is-unordered-list-item? [line]
  (boolean (re-matches #"^\s*[-+*]\s+.*$" line)))

(defn is-ordered-list-item? [line]
  (boolean (re-matches #"^\s*\d+[.)]\s+.*$" line)))

(defn is-list-item? [line]
  (or (is-unordered-list-item? line)
      (is-ordered-list-item? line)))

(defn is-begin-src? [line]
  (boolean (re-matches #"(?i)^\s*#\+BEGIN_SRC(?:\s+\w+)?.*$" line)))

(defn is-end-src? [line]
  (boolean (re-matches #"(?i)^\s*#\+END_SRC.*$" line)))

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
             (str/join "\n" (map #(str "<li>" (org-to-html (clean-list-item %)) "</li>")
                                 (take-while is-list-item? lines)))
             "\n</ul>")
        (str "<ol>\n"
             (str/join "\n" (map #(str "<li>" (org-to-html (clean-list-item %)) "</li>")
                                 (take-while is-list-item? lines)))
             "\n</ol>")))))

;; Extract language from BEGIN_SRC line
(defn extract-src-language [line]
  (let [language-match (re-find #"(?i)^\s*#\+BEGIN_SRC\s+(\w+)" line)]
    (when (and language-match (> (count language-match) 1))
      (second language-match))))

;; Process a code block for HTML output
(defn process-src-block-html [lines]
  (let [first-line   (first lines)
        language     (or (extract-src-language first-line) "")
        code-lines   (take-while (fn [line] (not (is-end-src? line)))
                                 (rest lines))
        code-content (str/join "\n" code-lines)]
    (str "<pre><code class=\"language-" language "\">"
         (-> code-content
             (str/replace #"&" "&amp;")
             (str/replace #"<" "&lt;")
             (str/replace #">" "&gt;"))
         "</code></pre>")))

;; Process a code block for Markdown output
(defn process-src-block-markdown [lines]
  (let [first-line   (first lines)
        language     (or (extract-src-language first-line) "")
        code-lines   (take-while (fn [line] (not (is-end-src? line)))
                                 (rest lines))
        code-content (str/join "\n" code-lines)]
    (str "```" language "\n"
         code-content
         "\n```")))

;; Detect if a line is part of a table
(defn is-table-row? [line]
  (boolean (re-matches #"^\s*\|.*\|\s*$" line)))

;; Detect if a line is a table separator
(defn is-table-separator? [line]
  (boolean (re-matches #"^\s*\|-+.*\|\s*$" line)))

;; Clean table cells
(defn clean-table-cells [row]
  (let [cells (-> row
                  str/trim
                  (subs 1 (dec (count row))) ;; Remove outer |
                  (str/split #"\s*\|\s*"))]
    (mapv str/trim cells)))

(defn preprocess-table [lines]
  (let [table-rows              (take-while is-table-row? lines)
        has-header              (and (> (count table-rows) 1)
                                     (is-table-separator? (second table-rows)))
        rows-without-separators (if has-header
                                  (concat [(first table-rows)]
                                          (drop 2 table-rows))
                                  table-rows)
        processed-rows          (mapv clean-table-cells rows-without-separators)]
    {:rows        processed-rows
     :has-header  has-header
     :table-lines table-rows}))

;; Process a table for HTML output
(defn process-table-html [lines]
  (let [{:keys [rows has-header]} (preprocess-table lines)]
    (if (empty? rows)
      ""
      (str "<table>\n"
           (if has-header
             (str "<thead>\n<tr>"
                  (str/join "" (map #(str "<th>" (org-to-html %) "</th>")
                                    (first rows)))
                  "</tr>\n</thead>\n")
             "")
           "<tbody>\n"
           (str/join "\n"
                     (map (fn [row]
                            (str "<tr>"
                                 (str/join "" (map #(str "<td>" (org-to-html %) "</td>") row))
                                 "</tr>"))
                          (if has-header (rest rows) rows)))
           "\n</tbody>\n</table>"))))

;; Process a table for Markdown output
(defn process-table-markdown [lines]
  (let [{:keys [rows has-header]} (preprocess-table lines)
        column-widths             (if (seq rows)
                                    (apply map (fn [& cells]
                                                 (apply max (map count cells)))
                                           rows)
                                    [])]
    (if (empty? rows)
      ""
      (str
       ;; Header row
       (when (seq rows)
         (str "| " (str/join
                    " | "
                    (map-indexed
                     (fn [i cell]
                       (format (str "%-" (nth column-widths i) "s")
                               (org-to-markdown cell)))
                     (first rows)))
              " |\n"))
       ;; Separator row
       (when (seq column-widths)
         (str "| " (str/join " | " (map #(apply str (repeat % "-")) column-widths)) " |\n"))
       ;; Data rows
       (when (seq rows)
         (str/join
          "\n"
          (map #(str "| " (str/join " | "
                                    (map-indexed
                                     (fn [i cell]
                                       (format (str "%-" (nth column-widths i) "s")
                                               (org-to-markdown cell)))
                                     %))
                     " |")
               (if has-header (rest rows) (rest rows)))))))))

;; Detect if a line is a quote line (starts with colon and space)
(defn is-quote-line? [line]
  (boolean (re-matches #"^\s*:\s.*$" line)))

;; Extract the content of a quote line
(defn extract-quote-content [line]
  (str/replace line #"^\s*:\s" ""))

;; Process consecutive quote lines for HTML output
(defn process-quote-lines-html [lines]
  (let [quote-lines   (take-while is-quote-line? lines)
        quote-content (map extract-quote-content quote-lines)]
    (str "<blockquote>\n"
         (str/join "\n" (map #(str "<p>" (org-to-html %) "</p>") quote-content))
         "\n</blockquote>")))

;; Process consecutive quote lines for Markdown output
(defn process-quote-lines-markdown [lines]
  (let [quote-lines   (take-while is-quote-line? lines)
        quote-content (map extract-quote-content quote-lines)]
    (str/join "\n" (map #(str "> " (org-to-markdown %)) quote-content))))

(defn content-to-html [content-lines]
  (if (empty? content-lines)
    ""
    (loop [remaining-lines content-lines
           result          []]
      (if (empty? remaining-lines)
        (str/join "\n" result)
        (let [current-line (first remaining-lines)]
          (cond
            ;; Source block processing
            (is-begin-src? current-line)
            (let [source-block-lines (take-while #(not (is-end-src? %))
                                                 remaining-lines)
                  all-block-lines    (take (+ (count source-block-lines) 1)
                                           remaining-lines)
                  source-html        (process-src-block-html all-block-lines)]
              (recur (drop (count all-block-lines) remaining-lines)
                     (conj result source-html)))
            ;; Quote line processing
            (is-quote-line? current-line)
            (let [quote-lines (take-while is-quote-line? remaining-lines)
                  quote-html  (process-quote-lines-html quote-lines)]
              (recur (drop (count quote-lines) remaining-lines)
                     (conj result quote-html)))
            ;; Table processing
            (is-table-row? current-line)
            (let [table-lines (take-while is-table-row? remaining-lines)
                  table-html  (process-table-html table-lines)]
              (recur (drop (count table-lines) remaining-lines)
                     (conj result table-html)))
            ;; List processing
            (is-list-item? current-line)
            (let [list-items (take-while is-list-item? remaining-lines)
                  list-html  (process-list-items list-items)]
              (recur (drop (count list-items) remaining-lines)
                     (conj result list-html)))
            ;; Regular paragraph text
            :else
            (recur (rest remaining-lines)
                   (conj result
                         (str "<p>" (org-to-html current-line) "</p>")))))))))

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
              (org-to-markdown cleaned)))
          lines)))

(defn content-to-markdown [content-lines]
  (if (empty? content-lines)
    ""
    (loop [remaining-lines content-lines
           result          []]
      (if (empty? remaining-lines)
        (str/join "\n\n" (remove empty? result))
        (let [current-line (first remaining-lines)]
          (cond
            ;; Source block processing
            (is-begin-src? current-line)
            (let [source-block-lines (take-while #(not (is-end-src? %))
                                                 remaining-lines)
                  all-block-lines    (take (+ (count source-block-lines) 1)
                                           remaining-lines)
                  source-md          (process-src-block-markdown all-block-lines)]
              (recur (drop (count all-block-lines) remaining-lines)
                     (conj result source-md)))
            ;; Quote line processing
            (is-quote-line? current-line)
            (let [quote-lines (take-while is-quote-line? remaining-lines)
                  quote-md    (process-quote-lines-markdown quote-lines)]
              (recur (drop (count quote-lines) remaining-lines)
                     (conj result quote-md)))
            ;; Table processing
            (is-table-row? current-line)
            (let [table-lines (take-while is-table-row? remaining-lines)
                  table-md    (process-table-markdown table-lines)]
              (recur (drop (count table-lines) remaining-lines)
                     (conj result table-md)))
            ;; List processing
            (is-list-item-md? current-line)
            (let [list-items (take-while is-list-item-md? remaining-lines)
                  list-md    (process-list-items-md list-items)]
              (recur (drop (count list-items) remaining-lines)
                     (conj result (str/join "\n" list-md))))
            ;; Regular paragraph text
            :else
            (if (str/blank? current-line)
              (recur (rest remaining-lines)
                     (conj result ""))
              (recur (rest remaining-lines)
                     (conj result (org-to-markdown current-line))))))))))

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

;; Updated to use format instead of boolean flags
(defn parse-org-file [file-path format]
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
                title          (case format
                                 :html     (org-to-html orig-title)
                                 :markdown (org-to-markdown orig-title)
                                 orig-title)
                ;; Store original title in path but converted title in headline
                path-title     (if (not= format :plain) orig-title title)
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

(defn filter-headlines
  "Returns a filtered sequence of headlines based on the provided criteria."
  [headlines min-level max-level title-pattern custom-id-pattern section-title-pattern]
  (into
   []
   (comp
    ;; Level filtering
    (filter #(and (or (nil? min-level) (>= (:level %) min-level))
                  (or (nil? max-level) (<= (:level %) max-level))))
    ;; Title filtering
    (filter (fn [headline]
              (or (nil? title-pattern)
                  (when-let [title (:title headline)]
                    (re-find title-pattern title)))))
    ;; Custom ID filtering
    (filter (fn [headline]
              (or (nil? custom-id-pattern)
                  (when-let [custom-id (get-in headline [:properties :custom_id])]
                    (re-find custom-id-pattern custom-id)))))
    ;; Section title filtering
    (filter (fn [headline]
              (or (nil? section-title-pattern)
                  (when-let [path (:path headline)]
                    (some (fn [path-item]
                            (when path-item
                              (re-find section-title-pattern path-item)))
                          path))))))
   ;; Process the headlines collection
   headlines))

(defn filter-headlines-by-section-custom-id [headlines section-custom-id-pattern]
  (if (or (empty? headlines) (nil? section-custom-id-pattern))
    headlines
    (let [;; Create an index of headlines by title for faster lookup
          headline-by-title   (reduce (fn [acc headline]
                                        (if-let [title (:title headline)]
                                          (assoc acc title headline)
                                          acc))
                                      {}
                                      headlines)
          ;; Find all custom IDs that match the pattern in one pass
          matching-custom-ids (into #{}
                                    (comp
                                     (filter #(get-in % [:properties :custom_id]))
                                     (filter #(re-find section-custom-id-pattern
                                                       (get-in % [:properties :custom_id])))
                                     (map #(get-in % [:properties :custom_id])))
                                    headlines)]
      ;; Filter headlines that are in a section with a matching custom ID
      (into []
            (filter (fn [headline]
                      (when-let [path (:path headline)]
                        (some (fn [path-title]
                                (when-let [section-headline (get headline-by-title path-title)]
                                  (when-let [custom-id (get-in section-headline [:properties :custom_id])]
                                    (contains? matching-custom-ids custom-id))))
                              path))))
            headlines))))

(defn prepare-for-output [options headlines format]
  (let [headlines (mapv #(-> % (update :path (fn [path]
                                               (when (seq path)
                                                 (butlast path)))))
                        headlines)
        headlines (if-not (:include-level options)
                    (mapv #(-> % (dissoc :level)) headlines)
                    headlines)]
    (case format
      "edn"  (mapv #(update % :path (fn [path] (when path (apply list path)))) headlines)
      "json" (mapv #(update % :path (fn [path] (or path []))) headlines)
      "yaml" (mapv #(update % :path (fn [path] (or path []))) headlines))))

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
    ;; Convert keywords in properties to strings
    (let [prepared-data
          (mapv (fn [headline]
                  (update headline :properties
                          #(into {} (map (fn [[k v]] [(name k) v]) %))))
                data)]
      (.write writer
              (yaml/generate-string
               prepared-data
               :dumper-options {:flow-style :block})))))

(defn clean-headline [headline format]
  (let [cleaned
        (-> headline
            (update :content
                    #(filterv (fn [line]
                                (and (not (str/blank? line))
                                     (not (is-comment? line)))) %))
            (update :properties
                    #(into {} (remove (fn [[_ v]] (str/blank? v)) %))))]
    (case format
      :html     (-> cleaned
                    (update :content content-to-html)
                    (update :title org-to-html))
      :markdown (-> cleaned
                    (update :content content-to-markdown)
                    (update :title org-to-markdown))
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
            format                    (cond
                                        (:html options)     :html
                                        (:markdown options) :markdown
                                        :else               :plain)
            output-format             (:format options)
            all-headlines             (parse-org-file file-path format)
            filtered-headlines        (-> all-headlines
                                          (filter-headlines
                                           min-level max-level
                                           title-pattern custom-id-pattern
                                           section-title-pattern)
                                          (filter-headlines-by-section-custom-id
                                           section-custom-id-pattern))
            clean-headlines           (mapv #(clean-headline % format) filtered-headlines)
            output-path               (str/replace file-path #"\.org$" (str "." output-format))
            prepared-headlines        (prepare-for-output options clean-headlines output-format)]
        (case output-format
          "json" (write-json prepared-headlines output-path)
          "edn"  (write-edn prepared-headlines output-path)
          "yaml" (write-yaml prepared-headlines output-path))
        (println (str (str/upper-case output-format)
                      " output written to " output-path))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
