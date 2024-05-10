;; init-calendar.el --- Initialize calendar configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2024 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Calendar configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Chinese calendar
;; `pC' can show lunar details
(when centaur-chinese-calendar
  (use-package cal-china-x
    :after calendar
    :autoload cal-china-x-setup
    :init (cal-china-x-setup)
    :config
    ;; Holidays
    (setq calendar-mark-holidays-flag t
          cal-china-x-important-holidays cal-china-x-chinese-holidays
          cal-china-x-general-holidays '(
                                         (holiday-fixed 1 1 "元旦")
                                         (holiday-lunar 1 1 "春节")
                                         (holiday-lunar 1 15 "元宵节")
                                         (holiday-fixed 3 8 "妇女节")
                                         (holiday-fixed 9 10 "教师节")
                                         (holiday-fixed 10 1 "国庆节")
                                         (holiday-fixed 10 2 "国庆节")
                                         (holiday-fixed 10 3 "国庆节")
                                         )
          holiday-other-holidays '(
                                  (holiday-lunar 12 30 "除夕")
                                  (holiday-lunar 1 2 "初二")
                                  (holiday-fixed 2 14 "情人节")
                                  (holiday-fixed 3 12 "植树节")                                  
                                  (holiday-fixed 4 1 "愚人节")
                                  (holiday-fixed 5 1 "劳动节")
                                  (holiday-fixed 5 4 "青年节")
                                  (holiday-lunar 5 5 "端午节")                                  
                                  (holiday-fixed 6 1 "儿童节")
                                  (holiday-lunar 7 7 "七夕节")
                                  (holiday-float 5 0 2 "母亲节")
                                  (holiday-float 6 0 3 "父亲节")
                                  (holiday-lunar 8 15 "中秋节")
                                  (holiday-lunar 9 9 "重阳节")                                  
                                  (holiday-float 11 4 4 "感恩节")
                                  (holiday-lunar 12 8 "腊八节")
                                  (holiday-fixed 12 25 "圣诞节")                                  
                                  (holiday-solar-term "清明" "清明节")
                                  (holiday-solar-term "小寒" "小寒")
                                  (holiday-solar-term "大寒" "大寒")
                                  (holiday-solar-term "立春" "立春")
                                  (holiday-solar-term "雨水" "雨水")
                                  (holiday-solar-term "惊蛰" "惊蛰")
                                  (holiday-solar-term "春分" "春分")
                                  (holiday-solar-term "谷雨" "谷雨")
                                  (holiday-solar-term "立夏" "立夏")
                                  (holiday-solar-term "小满" "小满")
                                  (holiday-solar-term "芒种" "芒种")
                                  (holiday-solar-term "夏至" "夏至")
                                  (holiday-solar-term "小暑" "小暑")
                                  (holiday-solar-term "大暑" "大暑")
                                  (holiday-solar-term "立秋" "立秋")
                                  (holiday-solar-term "处暑" "处暑")
                                  (holiday-solar-term "白露" "白露")
                                  (holiday-solar-term "秋分" "秋分")
                                  (holiday-solar-term "寒露" "寒露")
                                  (holiday-solar-term "霜降" "霜降")
                                  (holiday-solar-term "立冬" "立冬")
                                  (holiday-solar-term "小雪" "小雪")
                                  (holiday-solar-term "大雪" "大雪")
                                  (holiday-solar-term "冬至" "冬至")
                                   )
          calendar-holidays (append cal-china-x-important-holidays
                                    cal-china-x-general-holidays
                                    holiday-other-holidays))))
                                    
(setq calendar-week-start-day 1)

(provide 'init-calendar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-calendar.el ends here
