(ns repl-tooling.repl-client.protocols
  (:require [cljs.core.async :as async :refer [<! >!] :refer-macros [go-loop go]]
            [clojure.string :as str]))

(defprotocol Repl
  (treat-data [_ data])
  (send-command [_ command])
  (cmd-to-send [_ command]))

; (str/split "foo\nbar\nbaz\n" #"\n" 2)
; (str/split "a\n" #"\n" 2)

(defn- pause-buffer! [buffer] (swap! buffer assoc :paused true))
(defn- resume-buffer! [buffer] (swap! buffer assoc :paused false))
(defn- reset-contents! [buffer] (swap! buffer assoc :contents ""))

(defn- update-buffer-and-send [buffer out string]
  (let [[first-line rest] (str/split string #"\n" 2)
        contents (str (:contents @buffer) first-line)]
    (if rest
      (do
        (reset-contents! buffer)
        (go (>! out contents))
        (recur buffer out rest))
      (swap! buffer #(update % :contents str first-line)))))

(defn- treat-result [buffer out fragment data]
  (let [string (str data)]
    (if (:paused @buffer)
      (go (>! fragment string))
      (update-buffer-and-send buffer out string))))

(defn- write-into [socket data buffer fragment]
  (let [lines (-> data str str/trim (str/split #"\n"))
        to-send (butlast lines)]

    (pause-buffer! buffer)
    (go
     (doseq [line to-send]
       (.write socket (str line "\n"))
       (while (not (re-find #"#_=>" (str/join " " (async/alts! [fragment
                                                                (async/timeout 500)]))))))
     (reset-contents! buffer)
     (resume-buffer! buffer)
     (.write socket (str (last lines) "\n")))))

(def ^:private net (js/require "net"))
(defn connect-socket! [host port]
  (let [in (async/chan)
        fragment (async/chan)
        out (async/chan)
        buffer (atom {:paused false :contents ""})
        socket (doto (. net createConnection port host)
                     (.on "data" #(treat-result buffer out fragment %)))]
    (go-loop []
      (write-into socket (<! in) buffer fragment)
      (recur))
    [in out socket]))
