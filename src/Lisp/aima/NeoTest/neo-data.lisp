;;; 
 ;; PROFET Copyright 2015 (c) Data Mining and Machine Learning Group,
 ;; National University of Ireland Galway.  
 ;; This file is a part of PROFET  
 ;;
 ;; Licensed under the Apache License, Version 2.0 (the "License");
 ;; you may not use this file except in compliance with the License.
 ;; You may obtain a copy of the License at
 ;;
 ;;      http://www.apache.org/licenses/LICENSE-2.0
 ;;
 ;; Unless required by applicable law or agreed to in writing, software
 ;; distributed under the License is distributed on an "AS IS" BASIS,
 ;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 ;; See the License for the specific language governing permissions and
 ;; limitations under the License.
;;;

;;;; Created on 2008-11-17 20:34:19
;; M Madden, Sept 2007
;; Data for Phenylephrine taken from DrugModel spreadsheet.

;; Data on each line:
;; Time Dose-This-Min Recorded-Qty Measured-Serum-Conc Qty-Urine

;; Dose-This-Min: value is index of list (T F), so 0 => T

(defconstant experimental-data
  '(    (0 0 0.84 0.00001035 nil 74 70)   
(5 1 0 0.00000393 nil 74 70)
(10 1 0 0.00000276 nil 74 70)
(15 1 0 0.00000204 nil 74 70)
;;(20 1 0 0.00000183 nil)
;;(30 1 0 0.00000154 nil)
;;(45 1 0 0.00000122 nil)
;;(60 1 0 0.00000104 nil)
;;(90 1 0 0.00000075 nil)
;;(120 1 0 0.00000065 0.322)
;;(150 1 0 0.00000065 nil)
;;(180 1 0 0.00000049 nil)
;;(240 1 0 0.00000038 nil)
;;(300 1 0 0.0000003 nil)
;;(360 1 0 0.00000021 0.57)
;;(480 1 0 0.00000016 nil)
;;(720 1 0 nil 0.662)
;;(1440 1 0 nil .712)
;;(2880 1 0 nil .725)
    ))

(defconstant experimental-data-5min-model
  '(    (0 0 0.84 0.00001035 nil)   
(1 1 0 0.00000393 nil)
(2 1 0 0.00000276 nil)
(3 1 0 0.00000204 nil)
(4 1 0 0.00000183 nil)
(6 1 0 0.00000154 nil)
(9 1 0 0.00000122 nil)
(12 1 0 0.00000104 nil)
(18 1 0 0.00000075 nil)
(25 1 0 0.00000065 0.322)
(30 1 0 0.00000065 nil)
(36 1 0 0.00000049 nil)
(48 1 0 0.00000038 nil)
(60 1 0 0.0000003 nil)
(72 1 0 0.00000021 0.57)
(96 1 0 0.00000016 nil)
(145 1 0 nil 0.662)
;;(1440 1 0 nil .712)
;;(2880 1 0 nil .725)
    ))


(defconstant min-experimental-data

'((0 0 0.84 .00000767 nil)
  (5 1 0 0.00000235 nil)
  (10 1 0 0.00000118 nil)
  (15 1 0 0.00000109 nil)
  (20 1 0 0.00000077 nil)
  (30 1 0 0.00000081 nil)
  (45 1 0 0.0000005 nil)
  (60 1 0 0.00000056 nil)
  (90 1 0 0.00000044 nil)
  (120 1 0 0.00000036 0.28896)
  (150 1 0 0.00000032 nil)
  (180 1 0 0.00000029 nil)
  (240 1 0 0.00000026 nil)
  (300 1 0 0.00000018 nil)
  (360 1 0 0.00000011 0.48888)
  (480 1 0 0.00000013 nil)
  (720 1 0 nil 0.6048)
    ))

(defconstant digoxin-data
  '((0 0 0.252 4.61538461538462E-06 nil)
(6 1 0 4.0967032967033E-06 nil)
(36 1 0 2.32490842490842E-06 nil)
(60 1 0 1.54981684981685E-06 nil)
(120 1 0 7.33699633699634E-07 nil)
(180 1 0 5.11904761904762E-07 nil)
(240 1 0 4.47435897435897E-07 nil)
(300 1 0 4.24358974358974E-07 nil)
(360 1 0 4.12637362637363E-07 nil)
(420 1 0 4.03846153846154E-07 nil)
(480 1 0 3.95970695970696E-07 nil)
(540 1 0 3.88461538461538E-07 nil)
(600 1 0 3.81135531135531E-07 nil)
(660 1 0 3.73992673992674E-07 nil)
(720 1 0 3.66849816849817E-07 nil)
(780 1 0 3.5989010989011E-07 nil)
(960 1 0 3.4010989010989E-07 nil)
(1440 1 0 2.92124542124542E-07 nil)
    ))
