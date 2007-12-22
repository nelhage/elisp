;;; nethack-keys.el --- experimental dvorak key bindings

;; Copyright (C) 2002,2003,2005  Ryan Yeske and Shawn Betts

;; Author: Ryan C Yeske <rcyeske@vcn.bc.ca>
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(add-hook 'nethack-map-mode-hook
	  (lambda ()
	    (local-set-key "^" 'nethack-command-identify-trap)
;;	    (local-set-key "\M-[" 'nethack-command-cancel)
	    (local-set-key "\C-a" 'nethack-command-redo-previous)
;;	    (local-set-key "\C-c\C-c" 'nethack-command-quit-game)
	    (local-set-key "\C-e" 'nethack-command-kick)
;;fixme	    (local-set-key "\C-." 'nethack-command-wizard-search)
	    (local-set-key "\C-u" 'nethack-command-wizard-map)
	    (local-set-key "\C-i" 'nethack-command-create-monster)
	    (local-set-key "\C-c" 'nethack-command-identify-all-items)
	    (local-set-key "\C-r" 'nethack-command-wizard-show-location)
	    (local-set-key "\C-l" 'nethack-command-previous-message)
	    (local-set-key "\C-p" 'nethack-command-redraw-screen)
	    (local-set-key "\C-y" 'nethack-command-teleport-around-level)
	    (local-set-key "\C-k" 'nethack-command-wizard-teleport-between-levels)
;;fixme	    (local-set-key "\C-," 'nethack-command-wizard-wish)
;;	    (local-set-key "a" 'nethack-command-apply)
;;	    (local-set-key "A" 'nethack-command-remove-all-armor)
	    (local-set-key "x" 'nethack-command-southwest)
	    (local-set-key "X" 'nethack-command-southwest-until-ontop)
	    (local-set-key "\M-\C-b" 'nethack-command-southwest-until-near)
	    (local-set-key "j" 'nethack-command-close-door)
	    (local-set-key "J" 'nethack-command-call-monster)
	    (local-set-key "e" 'nethack-command-drop)
	    (local-set-key "E" 'nethack-command-drop-specific-item)
	    (local-set-key "." 'nethack-command-eat)
	    (local-set-key ">" 'nethack-command-engrave)
	    (local-set-key "u" 'nethack-command-fire)
	    (local-set-key "U" 'nethack-command-force-fight)
	    (local-set-key "i" 'nethack-command-move-until-near)
	    (local-set-key "I" 'nethack-command-move)
	    (local-set-key "d" 'nethack-command-west)
	    (local-set-key "D" 'nethack-command-west-until-ontop)
	    (local-set-key "\C-d" 'nethack-command-west-until-near)
	    (local-set-key "c" 'nethack-command-inventory)
	    (local-set-key "C" 'nethack-command-type-inventory)
	    (local-set-key "h" 'nethack-command-south)
	    (local-set-key "H" 'nethack-command-south-until-ontop)
;;	    (local-set-key "\C-h" 'nethack-command-south-until-near)
	    (local-set-key "t" 'nethack-command-north)
	    (local-set-key "T" 'nethack-command-north-until-ontop)
	    (local-set-key "\C-t" 'nethack-command-north-until-near)
	    (local-set-key "n" 'nethack-command-east)
	    (local-set-key "N" 'nethack-command-east-until-ontop)
	    (local-set-key "\C-n" 'nethack-command-east-until-near)
;;	    (local-set-key "m" 'nethack-command-move-no-pickup-or-fight)
;;	    (local-set-key "M" 'nethack-command-move-distance-no-pickup)
	    (local-set-key "b" 'nethack-command-southeast)
	    (local-set-key "B" 'nethack-command-southeast-until-ontop)
	    (local-set-key "\C-b" 'nethack-command-southeast-until-near)
	    (local-set-key "r" 'nethack-command-open)
	    (local-set-key "R" 'nethack-command-settings)
	    (local-set-key "l" 'nethack-command-pay)
	    (local-set-key "L" 'nethack-command-put-on)
	    (local-set-key "'" 'nethack-command-quaff)
	    (local-set-key "\"" 'nethack-command-select-ammo-for-quiver)
	    (local-set-key "p" 'nethack-command-read)
	    (local-set-key "P" 'nethack-command-remove-accessory)
	    (local-set-key "o" 'nethack-command-search)
	    (local-set-key "O" 'nethack-command-save-game)
	    (local-set-key "\M-o" 'nethack-command-offer)
	    (local-set-key "y" 'nethack-command-throw)
	    (local-set-key "Y" 'nethack-command-remove-single-armor)
	    (local-set-key "g" 'nethack-command-northeast)
	    (local-set-key "G" 'nethack-command-northeast-until-ontop)
	    (local-set-key "\C-g" 'nethack-command-northeast-until-near)
	    (local-set-key "k" 'nethack-command-version)
	    (local-set-key "K" 'nethack-command-version-and-history)
	    (local-set-key "," 'nethack-command-wield)
	    (local-set-key "<" 'nethack-command-wear-armor)
	    (local-set-key "q" 'nethack-command-swap-weapons)
	    (local-set-key "Q" 'nethack-command-explore-mode)
	    (local-set-key "f" 'nethack-command-northwest)
	    (local-set-key "F" 'nethack-command-northwest-until-ontop)
;;??	    (local-set-key "" 'nethack-command-northwest-until-near)
	    (local-set-key ";" 'nethack-command-zap-wand)
	    (local-set-key ":" 'nethack-command-cast-spell)
	    (local-set-key "V" 'nethack-command-up)
	    (local-set-key "W" 'nethack-command-down)
	    (local-set-key "z" 'nethack-command-what-is-symbol)
	    (local-set-key "Z" 'nethack-command-help)
;;	    (local-set-key "&" 'nethack-command-command-help)
;;	    (local-set-key "!" 'nethack-command-shell-escape)
;;	    (local-set-key "\\" 'nethack-command-show-discoveries)
	    (local-set-key "v" 'nethack-command-rest-one-move)
	    (local-set-key " " 'nethack-command-rest-one-move)
	    (local-set-key "S" 'nethack-command-look-here)
	    (local-set-key "s" 'nethack-command-what-is-map-piece)
	    (local-set-key "w" 'nethack-command-pick-up)
;;	    (local-set-key "@" 'nethack-command-toggle-pickup)
;;	    (local-set-key ")" 'nethack-command-show-wielded-weapon)
	    (local-set-key "/" 'nethack-command-show-worn-armor)
	    (local-set-key "]" 'nethack-command-show-worn-rings)
	    (local-set-key "_" 'nethack-command-show-worn-amulet)
;;	    (local-set-key "(" 'nethack-command-show-tool-in-use)
	    (local-set-key "*" 'nethack-command-show-all-equipment-in-use)
	    (local-set-key "$" 'nethack-command-count-gold)
	    (local-set-key "}" 'nethack-command-list-known-spells)))
