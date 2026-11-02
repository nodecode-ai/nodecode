;;;; gif-test.lisp --- a GIF's first frame read here, as PIL shows it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is GIF-DECODE's contract: a palette frame, an
;;;; interlaced one, one with a transparent index, gray, two-colour, an
;;;; animation's first frame, a frame at an offset on a wider screen, one
;;;; that overhangs its screen and a file cut short each decode to exactly
;;;; the pixels PIL shows (each fixture written and read back by PIL, as RGB
;;;; or, with a transparent index, RGBA, the pixels kept as their SHA-256);
;;;; bytes that open no GIF, a GIF with no frame and a screen past
;;;; *IMAGE-MAX-PIXELS* are refused as GIF-MALFORMED, and through GIF-PIXELS
;;;; as IMAGE-UNFIT; and FIT-IMAGE shrinks a GIF past 2000 px a side with no
;;;; transcoder on the box.

(in-package #:nodecode.test)

(defparameter *gif-fixtures*
  '(    ("palette" 9 6 3 "7bf595bf8ccf26afaff20508d5c4106713d5999cc7c5b5c91a55a016767860df"
     "R0lGODdhCQAGAIQAAOr24truq+Tl7bP3uYDilHPuiRvXSwncKAD4Lk3LMst61DOCK7JigmBkbyNfOO80vc9MXNIavcIAnns2oJAbYyVAWzpMFAA8WxsRSAwbNkMBajcGKBIBSwAAAAAAAAAAACwAAAAACQAGAAAIOwALDBAAYEABBAgMECAYgICBAwgSKJjIoMECBwsaRHgQgcIGDBk4TIggweMGDhg0RIDA0oKDCxUmPAgIADs=")
    ("interlaced" 9 11 3 "c24ecd18ef208fe893c5f0eb2e9a2439c30203b758610a95ce506029554d91cb"
     "R0lGODdhCQALAIQAAOf8wOro6Z7ywtbT163Hz0TzfRryaV/PmgXcQ+N7qvBfy5F3iKJgiBmjRC10bS5kMOMyvPA3SopLhYUpV/MfYtMROK0AqKsOGVJJihhGURcxFk0KQyIDKQsZMAgAIwAAACwAAAAACQALAAAIYgAFABhI4EABAwYEEAgwQMABAw0QLGR44ACCiwQUKEiwoIHHBxoTMJjwoOQECBAoSNigQcODkxQqTODAwcMGCxQiXNhQs8PNmDs7dMCAwUKEChc0ZMiAQQKECBEmOHBAVGNAADs=")
    ("transparent" 9 6 4 "933befa79a6b11b4209967a12e783824452d0615f33f2fc4b71c0df7e11ad55c"
     "R0lGODlhCQAGAIEAAPr6+sgeKABayAAAACH5BAEAAAAALAAAAAAJAAYAAAgdAAEIHAhAwMAACAUoNAggoUKCCxkKXEiw4MOKAQEAOw==")
    ("gray" 9 6 3 "f2d72a25a127c73e6f87e01f19ddb7a1836f04b09ca6914a1c5e228591b02e47"
     "R0lGODdhCQAGAIUAAAkJCQwMDBcXFxsbGyIiIiUlJScnJykpKS0tLTExMTQ0NEBAQEJCQkVFRUlJSU5OTlBQUFxcXF9fX2NjY2RkZGdnZ2hoaG9vb3Jycnd3d3p6enx8fH19fYSEhIeHh5GRkZKSkpOTk5ubm52dnZ+fn6Kioq+vr7u7u8bGxtfX19jY2N3d3d/f3+Xl5efn5/Ly8vf39wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAACQAGAAAIPwBPrHjhogUKEyI6qGABIwWJDx08gCAxokSICRUiPNCAwQKFBggKCDDA4IKEBQcCAECgAMKGDBMIDEjgoAKHgAA7")
    ("two-colour" 9 6 3 "7befcd37ac57b1a306edfed6e7a4f9aeacd9b7f66421d0db60fff5061aea01a6"
     "R0lGODdhCQAGAIcAAAAAAAEBAQICAgMDAwQEBAUFBQYGBgcHBwgICAkJCQoKCgsLCwwMDA0NDQ4ODg8PDxAQEBERERISEhMTExQUFBUVFRYWFhcXFxgYGBkZGRoaGhsbGxwcHB0dHR4eHh8fHyAgICEhISIiIiMjIyQkJCUlJSYmJicnJygoKCkpKSoqKisrKywsLC0tLS4uLi8vLzAwMDExMTIyMjMzMzQ0NDU1NTY2Njc3Nzg4ODk5OTo6Ojs7Ozw8PD09PT4+Pj8/P0BAQEFBQUJCQkNDQ0REREVFRUZGRkdHR0hISElJSUpKSktLS0xMTE1NTU5OTk9PT1BQUFFRUVJSUlNTU1RUVFVVVVZWVldXV1hYWFlZWVpaWltbW1xcXF1dXV5eXl9fX2BgYGFhYWJiYmNjY2RkZGVlZWZmZmdnZ2hoaGlpaWpqamtra2xsbG1tbW5ubm9vb3BwcHFxcXJycnNzc3R0dHV1dXZ2dnd3d3h4eHl5eXp6ent7e3x8fH19fX5+fn9/f4CAgIGBgYKCgoODg4SEhIWFhYaGhoeHh4iIiImJiYqKiouLi4yMjI2NjY6Ojo+Pj5CQkJGRkZKSkpOTk5SUlJWVlZaWlpeXl5iYmJmZmZqampubm5ycnJ2dnZ6enp+fn6CgoKGhoaKioqOjo6SkpKWlpaampqenp6ioqKmpqaqqqqurq6ysrK2tra6urq+vr7CwsLGxsbKysrOzs7S0tLW1tba2tre3t7i4uLm5ubq6uru7u7y8vL29vb6+vr+/v8DAwMHBwcLCwsPDw8TExMXFxcbGxsfHx8jIyMnJycrKysvLy8zMzM3Nzc7Ozs/Pz9DQ0NHR0dLS0tPT09TU1NXV1dbW1tfX19jY2NnZ2dra2tvb29zc3N3d3d7e3t/f3+Dg4OHh4eLi4uPj4+Tk5OXl5ebm5ufn5+jo6Onp6erq6uvr6+zs7O3t7e7u7u/v7/Dw8PHx8fLy8vPz8/T09PX19fb29vf39/j4+Pn5+fr6+vv7+/z8/P39/f7+/v///ywAAAAACQAGAAAIGwD/CRz4D8BAAAgNIhSosCBBhQYPLnzokOC/gAA7")
    ("animation" 8 5 3 "4abc241163c45a0e1219c2f0258f99b6995070ef3045e32710122d32c5277d91"
     "R0lGODlhCAAFAIMAANr4zJ/iu+S/zjz+WD3TKtZ0votUYxihIOYjtJI8P3Q1VsQCrjs7fAopPT0IJAkACiH/C05FVFNDQVBFMi4wAwEAAAAh+QQACgAAACwAAAAACAAFAAAILAADABgYYAABAgEECAhA4IDDAggKJGhwoIEBBAsUOHjwwMECiAkUNGDAAEFAACH5BAEKABAALAAAAAAIAAUAg87x0KT7ptzKz1rmYgHrH7ipsu1VuiWWQxpJDdMrsc4Pao0ngi07OBMzZCgfK0kJawgqAAMAEAAgwAACBAoMLHCQwAEDEBcwQMCAQQIFChxofJDgIgMHDRosSBAQACH5BAEKABAALAAAAAAIAAUAg+nw3sHKn4zanSj7LRbQJMifuKxoqDCKNdk7meUb0agjitQMkT45dw06MEQAMAAAOAgrAAUAGChggEEBBQAEOECgYYICCBgcaNDgQIIFChw4eOBAwQIEDigyMJAgIAA7")
    ("offset" 11 7 4 "28e5e02e03dd1b8bd83b0aaefedb4310942b6612b30a02f35940d4bc9fc1f530"
     "R0lGODlhCwAHAIEAAPr6+gBayMgeKAAAACH5BAEAAAIALAMAAgAGAAQAAAgRAAEIBBBAoIAACAsmFLhQYEAAOw==")
    ("overhang" 9 6 3 "e5d0d6881c651991c610b5dcebec1d8562909a046a8ce4f3345501330f2c2b87"
     "R0lGODdhBAADAIEAAPr6+sgeKABayAAAACwAAAAACQAGAAAIHQABCBwIQMDAAAgFKDQIIKFCggsZClxIsODDigEBADs=")
    ("cut-short" 40 30 3 "e85f822bc56e5cb775f533ff53857bf92038b76fa0aa014502e99d9e89885988"
     "R0lGODdhKAAeAIcAAPr66fX90eH74t35vPXv0+Hwz/nl4d/j0sv+xrf9ssTtzLDvs5v8roH9kprsrpLshPjd2N7dyvTQ6tvN1/K85Puy3OG82uCw1snVys/LvqvVsYTWjMHBuMO3v6HDtqa9cmn+jG7noFv7IlvlLDD6OzTrQDPjcDXkMQr8Qw/vSRD+Fw7yIA3lVRHnKA7kCQHlFWTTgUDYcUrWMC7PKWi8Vl+vPjrCRjO1KiHZPAPXOSHGNAvLJBy8PRywIQm4LQizGPKh5OmW6+aixuCTyvOK39mD5fOGud2EYL2P1a2XeIOpW8CDu6yFOYt/hYuBMPtz2fRi1eNv3Nhg5e1rpdpcofNnTdZoMbJys5Jqtb9bz55g07h0M61dKJVuUpRcWWqkTFiUYj+bXjucImqDrEGAfFGDOTGBMA+aYR+hMg+gGR+WDhOGlSKFPRSAGQCIJmtwnjZyrzlvYFZuK2BaJTFzHz9jIRRwmA1vUA9gQgFePglzIRRsGhdeGQNgHvFJ1OVP39pS4vU63dQ/3uNQaN9Db+85qOE3XfQs0fQi0NQt3NIgrv4pgekqgc4odc8mQbZRv7Y9u4dNt4g+papKTKc8QYFISrQz2rQlvZQnpHsopb0kOqEnLZAvOoAqUPsU8PAUtvAKyN4Wv+ULzvcFuf8Au/8Ar+YBxNQA2O4WePwOjtcaZtISZvgEjvEJZeADeM0EbbIY268Xp7sG37cErYkVqpMJsYoAq3oCoLsZWrwLUa8BP5YZXY0NRYkEO2dKxWdGXT9Nd0M8jVlQK008JDZMJy9AL18sm2gsXGEqIU0vFzgshz8vLTsqECsrAxBTZAZVPgs/dwVCTB9UIw1OIhM/IA84JBozYwAyVBsmYQMjShkxIAkwLBgjJlwYnWQWTV8FllgDbmsTH1AWJmQGLVEDJzQWcjsJgD0AijYAZDcVJz4OGDcEPj0GHAwXUw8XNxwNSAEPQRgaFyEPGAEYIQAOCxEBWQIBSxcCNgEANiQDHAIEHAAAAAAAAAAAAAAAAAAAACwAAAAAKAAeAEAI/wBBNHjAIMGAAgoiQAAAQIKAAAAEDBDgYAGIEDBimDCRooQKFip25FBBksQJEUWGELkQJMiTChWGGEEipEOSD0ma0ADzxQwaM2p+9Egz1E0aNXLQlAlT5osTUZ48lQKFiJWiQo1iveq1qxendOLkxePGjN5YfPjuvUNXrpw3dN9uSYolC9aiRYQIGTLkiBInSsgozUmWLN48PtuyPYt2DZoyx92MRcoUSUukRJYsBQr0x4EDDKAnGIAw+oCEgxkShkiwIQSNByZulCjRQoeKHD1auLjhQoaIETWyRBH0RxAiP4WMTBlEhYoXL512CYuzjA8zadW2TXu2Dc+0Yc6c/f869uuNpF9eQJFKxSoVKlWOXuXSxUsXOXHq0qmTd08fvnz32GNPPeWgc445mdxySy20zCLKKZ4cUUUVR2zBBRdMMOGEHHWYUQcdbNwRBx53rOHMGnDYAYcvK2qBhRZIFCGFFE8EQYQQDDiQQAICRGCAAQRIEMCQBRCAgAIKMOBBCA2AAMJGLLyAggoo5IDCCy60gMMJMsiARAYZcHABBRkEQYEFFwgBgRAeaPDBBh5soJENJvDAwwspuLHDDz708MMON8wgRg01BCEFEX9A8cQTii5BxRJXPIKFF1108UsZYLBRhx5i7JGHHnq0oUcfnNJRhhxNNPHGIaEcAoofnxT/skgjg7yiyC67dMKJN8UQxkwx1GjDTTzZzLPNO9ZY8wswxxiTCSaYXIKIep+Mwp4rrTSySy6beOMtO/l4Ew892tCDDzz3pLPOO+bUY0xcDdYCiSmkLOIee6o0kosmm/TiDTnqMKOONs3Ik40218AzYDvYlHPON934gkkml9QCiyxQVTFIFY5YwYXHc1Qy")
    ("wide" 2400 16 3 "d8b443032200e143b1c49820b6d78c32d519553a3f63a38f48e8bc31da084f0a"
     "R0lGODdhYAkQAIAAAAAAAAAAACwAAAAAYAkQAEAI/wABCBxIsKDBgwgTKlzIsKHDhxAjSpxIsaLFixgzatzIsaPHjyBDihxJsqTJkyhTqlzJsqXLlzBjypxJs6bNmzhz6tzJs6fPn0CDCh1KtKjRo0iTKl3KtKnTp1CjSp1KtarVq1izat3KtavXr2DDih1LtqzZs2jTql3Ltq3bt3Djyp1Lt67du3jz6t3Lt6/fv4ADCx5MuLDhw4gTK17MuLHjx5AjS55MubLly5gza97MubPnz6BDix5NurTp06hTq17NurXr17Bjy55Nu7bt27hz697Nu7fv38CDCx9OvLjx48iTK1/OvLnz59CjS59Ovbr169iza9/Ovbv37+DDiz4fT768+fPo06tfz769+/fw48ufT7++/fv48+vfz7+///8ABijggAQWaOCBCCao4IIMNujggxBGKOGEFLoWEAA7"))
  "Each fixture's name, width, height, channels, the SHA-256 of the pixels
PIL shows, and the file as base64.")

(defun gif-fixture (name)
  (cl-base64:base64-string-to-usb8-array (sixth (assoc name *gif-fixtures* :test #'string=))))

(deftest gif-decodes-every-form-to-pils-pixels ()
  (loop for (name width height channels digest) in *gif-fixtures*
        do (multiple-value-bind (pixels w h c) (nle::gif-decode (gif-fixture name))
             (is (and (eql w width) (eql h height) (eql c channels))
                 (format nil "~a is ~dx~d of ~d channels, not ~dx~d of ~d" name width height channels w h c))
             (is (string= digest (subseq (nlk::sha256-text pixels) 7))
                 (format nil "~a decodes to the pixels PIL shows" name)))))

(deftest gif-pixels-hands-fit-image-its-pixels ()
  (is-present (pixels (nle::gif-pixels (gif-fixture "transparent"))) "the PIXELS of a GIF with a transparent index"
    (is (nle::pixels-p pixels) "a PIXELS")
    (is-shape pixels (nle::pixels-width 9) (nle::pixels-height 6) (nle::pixels-channels 4 "RGBA"))))

(deftest gif-refuses-what-it-cannot-read ()
  (let ((palette (gif-fixture "palette")))
    (signals-error nle::gif-malformed (nle::gif-decode (octets 1 2 3 4)))
    ;; A screen with no frame on it is refused.
    (let ((report (refusal-text nle::gif-malformed
                    (nle::gif-decode (concatenate '(vector (unsigned-byte 8))
                                                  (replace (subseq palette 0 13) (octets 0) :start1 10)
                                                  (octets #x3B))))))
      (is (search "holds no frame" report) report))
    (signals-error nle::image-unfit (nle::gif-pixels (octets 1 2 3 4)))
    (let ((nle::*image-max-pixels* 50))
      ;; A screen past the ceiling is refused before its codes are read.
      (let ((report (refusal-text nle::gif-malformed (nle::gif-decode palette))))
        (is (search "past the 50 pixels" report) report)))))

(deftest fit-image-shrinks-a-gif-with-no-transcoder ()
  (let ((nlt:*png-transcoders* '()))
    (multiple-value-bind (octets media-type width height) (nle::fit-image (gif-fixture "wide"))
      (is (equal (list 2000 13) (list width height)) "2400 x 16 shrunk to 2000 x 13")
      (is (member media-type '("image/png" "image/jpeg") :test #'equal) "re-encoded as PNG or JPEG")
      (is (equal (list 2000 13) (multiple-value-list (nlt:image-dimensions octets))) "the bytes sent are that size"))))
