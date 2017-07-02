(ns hello.model
  (:use korma.db))

(defdb db (sqlite3 {:db "resource/db/sqlite.db"}))
