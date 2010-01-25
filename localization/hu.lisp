;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(def localization hu
  (authenticated-session.web-application "alkalmazás")
  (class-name.authenticated-session "belépés")
  (class-name.authentication-instrument "azonosító eszköz")
  (class-name.binary-relationship "bináris kapcsolat")
  (class-name.binary-relationship-end "bináris kapcsolat végződés")
  (class-name.message-queue "üzenetsor")
  (class-name.n-ary-relationship "n-es kapcsolat")
  (class-name.n-ary-relationship-end "n-es kapcsolat végződés")
  (class-name.persistent-message "perzisztens üzenet")
  (class-name.persistent-process "megszakítható folyamat")
  (class-name.relationship "kapcsolat")
  (class-name.relationship-element "kapcsolat elem")
  (class-name.relationship-end "kapcsolat végződés")
  (class-name.subject "alany")
  (class-name.technical-subject "technikai alany")
  (class-name.wait-for-message "üzenetre várakozás")
  (class-name.wait-for-subject "alanyra várakozás")
  (class-name.wait-for-timestamp "időpontra várakozás")
  (class-name.wait-reason "várakozási ok")
  (process-state.broken "tönkrement")
  (process-state.cancelled "elvetve")
  (process-state.failed "hibás")
  (process-state.finished "befeződött")
  (process-state.in-progress "folyamatban")
  (process-state.initializing "felkészítés alatt")
  (process-state.paused "felfüggesztve")
  (process-state.running "fut")
  (slot-name.authenticated-sessions "belépések")
  (slot-name.authenticated-subject "azonosított alany")
  (slot-name.authentication-instruments "azonosító eszközök")
  (slot-name.description "leírás")
  (slot-name.documentation "dokumentáció")
  (slot-name.editable "módosítható")
  (slot-name.effective-sessions "effektív belépések")
  (slot-name.effective-subject "effektív alany")
  (slot-name.http-user-agent "http kliens")
  (slot-name.login-at "belépés időpontja")
  (slot-name.login-disabled "belépés letiltva")
  (slot-name.logout-at "kilépés időpontja")
  (slot-name.name "név")
  (slot-name.process-state "folyamat állapot")
  (slot-name.remote-ip-address "kliens IP címe")
  (slot-name.required "kötelező")
  (slot-name.status "állapot")
  (slot-name.subject "alany")
  (slot-name.type "típus")
  (slot-name.unique "egyedi")
  (slot-name.web-session-id "web session azonosító")
  (status.alive "élő")
  (status.crashed "lefagyott")
  (status.expired "lejárt")
  (status.logged-out "kilépve")
  (status.shutdown "leállítva")
  (type-name.boolean "logikai érték")
  (type-name.date "dátum")
  (type-name.email-address "email cím")
  (type-name.integer "egész szám")
  (type-name.integer-16 "16 bites egész szám")
  (type-name.integer-32 "32 bites egész szám")
  (type-name.integer-64 "64 bites egész szám")
  (type-name.ip-address "IP cím")
  (type-name.member "felsorolt típus")
  (type-name.number "szám")
  (type-name.phone-number "telefonszám")
  (type-name.serialized "sorosított")
  (type-name.standard-text "rövid szöveg")
  (type-name.symbol "szimbólum")
  (type-name.t "bármi")
  (type-name.text "szöveg")
  (type-name.time "idő")
  (type-name.timestamp "időpont")
  (type-name.url-text "URL"))
