;;;; image-fit-test.lisp --- an image as a provider takes it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims for image-fit.lisp and the doors it guards:
;;;;
;;;;   F1. The PNG reader opens every colour type at every depth PNG has,
;;;;       tRNS and Adam7 among them, to the samples an independent encoder
;;;;       wrote; what the writer writes reads back as its own pixels, under
;;;;       the checksums an independent encoder computes.
;;;;   F2. JPEG, baseline or progressive, decodes red first and gray as one
;;;;       channel; the EXIF orientation is read in either byte order, and
;;;;       all eight turn the pixels upright.
;;;;   F3. The shrink is by area, alpha premultiplied, the shape kept.
;;;;   F4. FIT-IMAGE sends an image within both ceilings as itself; past the
;;;;       side a photo as JPEG and a flat picture as PNG, upright; past the
;;;;       bytes at lower qualities, then smaller; a header past
;;;;       *IMAGE-MAX-PIXELS* is refused unopened; a format no codec here
;;;;       reads goes through the box's transcoder, or as itself within what
;;;;       a provider takes whole.
;;;;   F5. LOOK's receipt says what was sent, and every image a turn stages
;;;;       is fitted first.

(in-package #:nodecode.test)

;;; --- fixtures ----------------------------------------------------------------

(defun hex-octets (hex)
  "The octets HEX spells, two digits each."
  (let ((out (make-array (floor (length hex) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length out) out)
      (setf (aref out i) (parse-integer hex :start (* 2 i) :end (* 2 (1+ i)) :radix 16)))))

(defun base64-octets (text)
  "The octets the base64 TEXT carries."
  (cl-base64:base64-string-to-usb8-array text))

(defun test-pixels (width height channels sample)
  "WIDTH x HEIGHT PIXELS of CHANNELS samples, each (SAMPLE X Y K)."
  (let* ((pixels (nle::blank-pixels width height channels))
         (data (nle::pixels-data pixels)))
    (dotimes (y height pixels)
      (dotimes (x width)
        (dotimes (k channels)
          (setf (aref data (+ k (* channels (+ x (* y width))))) (funcall sample x y k)))))))

(defun pixel-at (pixels x y)
  "The samples of PIXELS at X, Y, as a list."
  (let ((channels (nle::pixels-channels pixels))
        (at (+ x (* y (nle::pixels-width pixels)))))
    (coerce (subseq (nle::pixels-data pixels) (* channels at) (* channels (1+ at))) 'list)))

(defun near (expected actual &optional (slack 12))
  "Whether each of the samples ACTUAL is within SLACK of EXPECTED's."
  (and (= (length expected) (length actual))
       (every (lambda (e a) (<= (abs (- e a)) slack)) expected actual)))

(defun photo-pixels (width height)
  "WIDTH x HEIGHT RGB pixels as a camera takes them: smooth light, fine grain."
  (let ((state 1))
    (test-pixels width height 3
                 (lambda (x y k)
                   (setf state (mod (+ (* state 1103515245) 12345) (expt 2 31)))
                   (max 0 (min 255 (+ (floor (* 200 (+ x y (* 9 k))) (+ width height 27))
                                      (- (ldb (byte 5 16) state) 16))))))))

(defun flat-pixels (width height)
  "WIDTH x HEIGHT RGB pixels as a diagram draws them: four flat quarters."
  (test-pixels width height 3
               (lambda (x y k)
                 (nth k (if (< x (floor width 2))
                            (if (< y (floor height 2)) '(255 255 255) '(20 60 200))
                            (if (< y (floor height 2)) '(240 40 40) '(0 0 0)))))))

;;; The JPEGs and the GIF are PIL's: two flat halves, red and blue, 16 x 8, at
;;; quality 95 with colour unsubsampled; the gray one 60 and 200; the EXIF
;;; ones carry orientation 6 in Intel byte order and 8 in Motorola's; the
;;; progressive one is what cl-jpeg could not read. The arithmetic-coded
;;; one, ImageMagick's, flat red, is a form no codec here reads.
(defparameter *fit-images*
  '((jpeg-colour "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAIBAQEBAQIBAQECAgICAgQDAgICAgUEBAMEBgUGBgYFBgYGBwkIBgcJBwYGCAsICQoKCgoKBggLDAsKDAkKCgr/2wBDAQICAgICAgUDAwUKBwYHCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgr/wAARCAAIABADAREAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD5nr+dz/ZA+a6/6GD/AAvP/9k=")
    (jpeg-gray "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAIBAQEBAQIBAQECAgICAgQDAgICAgUEBAMEBgUGBgYFBgYGBwkIBgcJBwYGCAsICQoKCgoKBggLDAsKDAkKCgr/wAALCAAIABABAREA/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/9oACAEBAAA/APzvr+jCv//Z")
    (jpeg-exif-6-intel "/9j/4AAQSkZJRgABAQAAAQABAAD/4QAiRXhpZgAATU0AKgAAAAgAAQESAAMAAAABAAYAAAAAAAD/2wBDAAIBAQEBAQIBAQECAgICAgQDAgICAgUEBAMEBgUGBgYFBgYGBwkIBgcJBwYGCAsICQoKCgoKBggLDAsKDAkKCgr/2wBDAQICAgICAgUDAwUKBwYHCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgr/wAARCAAIABADAREAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD5nr+dz/ZA+a6/6GD/AAvP/9k=")
    (jpeg-progressive "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAIBAQEBAQIBAQECAgICAgQDAgICAgUEBAMEBgUGBgYFBgYGBwkIBgcJBwYGCAsICQoKCgoKBggLDAsKDAkKCgr/2wBDAQICAgICAgUDAwUKBwYHCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgr/wgARCAAIABADAREAAhEBAxEB/8QAFAABAAAAAAAAAAAAAAAAAAAAB//EABYBAQEBAAAAAAAAAAAAAAAAAAgHCf/aAAwDAQACEAMQAAABM52yDXQwL//EABQQAQAAAAAAAAAAAAAAAAAAABD/2gAIAQEAAQUCP//EABQRAQAAAAAAAAAAAAAAAAAAABD/2gAIAQMBAT8BP//EABQRAQAAAAAAAAAAAAAAAAAAABD/2gAIAQIBAT8BP//EABQQAQAAAAAAAAAAAAAAAAAAABD/2gAIAQEABj8CP//EABQQAQAAAAAAAAAAAAAAAAAAABD/2gAIAQEAAT8hP//aAAwDAQACAAMAAAAQA//EABQRAQAAAAAAAAAAAAAAAAAAABD/2gAIAQMBAT8QP//EABQRAQAAAAAAAAAAAAAAAAAAABD/2gAIAQIBAT8QP//EABQQAQAAAAAAAAAAAAAAAAAAABD/2gAIAQEAAT8QP//Z")
    (jpeg-exif-8-motorola "/9j/4QAiRXhpZgAATU0AKgAAAAgAAQESAAMAAAABAAgAAAAAAAD/4AAQSkZJRgABAQAAAQABAAD/2wBDAAIBAQEBAQIBAQECAgICAgQDAgICAgUEBAMEBgUGBgYFBgYGBwkIBgcJBwYGCAsICQoKCgoKBggLDAsKDAkKCgr/2wBDAQICAgICAgUDAwUKBwYHCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgr/wAARCAAIABADAREAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD5nr+dz/ZA+a6/6GD/AAvP/9k=")
    (jpeg-arithmetic "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgICAgMCAgIDAwMDBAYEBAQEBAgGBgUGCQgKCgkICQkKDA8MCgsOCwkJDRENDg8QEBEQCgwSExIQEw8QEBD/2wBDAQMDAwQDBAgEBAgQCwkLEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBD/yQARCAAIABADAREAAhEBAxEB/8wACgAQEAUBEBEF/9oADAMBAAIRAxEAPwD/AM/wDqQM1Aj/2Q=="))
  "Image files by name, base64.")

(defun fit-fixture (name)
  "The bytes of the image NAME in *FIT-IMAGES*."
  (base64-octets (second (assoc name *fit-images*))))

;;; --- F1: PNG ----------------------------------------------------------------------
;;; Each PNG is written by an encoder of the test's own, apart from the one
;;; under test (Python's zlib under hand-written rows, each row under the
;;; next of the five filters, the rgb-8 image's data split across two IDAT
;;; chunks); beside it, WIDTH, HEIGHT and the samples the PNG spec makes of
;;; it as this reader lands them: one channel for gray, three for RGB or a
;;; palette, four with alpha or tRNS, sixteen bits to their high byte, fewer
;;; than eight scaled to 255.

(deftests png-reader-opens (png width height channels samples)
  (let ((pixels (nle::png-pixels (base64-octets png))))
    (is-shape pixels (nle::pixels-width width) (nle::pixels-height height) (nle::pixels-channels channels)
      (nle::pixels-data equalp (hex-octets samples))))
  (gray-1 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADAQAAAABzTfhVAAAADklEQVR42mMIYPzBFAEABN4BpK+fRL0AAAAASUVORK5CYII=" 5 3 1
   "00ff00ff00ffffffffff00ff00ff00")
  (gray-2 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADAgAAAAA07YKFAAAAEUlEQVR42mOwZGAMfc0U4wAACCoCGSJWQr4AAAAASUVORK5CYII=" 5 3 1
   "00ffaa55005555555555aaff0055aa")
  (gray-4 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADBAAAAAC7rXclAAAAFElEQVR42mNgzjzAGFlRzxRWMQEAFVMD3rRbpvQAAAAASUVORK5CYII=" 5 3 1
   "00336699cc5599dd1155aaff4499ee")
  (gray-8 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADCAAAAAB+XZokAAAAFklEQVR42mPgNQqvWciYoQoETNEgAAAm7AR6tvH/dwAAAABJRU5ErkJggg==" 5 3 1
   "0d32577ca1688db2d7fcc3e80d3257")
  (gray-16-trns "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADEAAAAAAuzUZnAAAAAnRSTlONKM0Lg8QAAAAeSURBVHjaY+BlMOIOF6tRXKjDmCGryg2BTNGyMAgAW9oFwtaeRlMAAAAASUVORK5CYII=" 5 3 4
   "0d0d0dff323232ff575757ff7c7c7cffa1a1a1ff686868ff8d8d8d00b2b2b2ffd7d7d7fffcfcfcffc3c3c3ffe8e8e8ff0d0d0dff323232ff575757ff")
  (rgb-8 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADCAIAAADUVFKvAAAAGElEQVR42mPgEmE1EmGNEmFtEmFdJcLKyBXFqsEhk58eAAAAGElEQVQgB0dMDG4MDG5yDG42DG5RDG4VAIv4Bk7z/xc6AAAAAElFTkSuQmCC" 5 3 3
   "0a14053214055a1405821405aa14050a5a05325a235a5a41825a5faa5a7d0aa00532a0415aa07d82a0b9aaa0f5")
  (rgb-16 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADEAIAAACExI7sAAAASElEQVR42mPgYhQxZk014hCxYs2J4hdxZC1uEhPxYK1aJSviz9rIyMUSZcaaocHOwC7HjkkyMTC7MTMwg0g5MGkDJqPAZAUzAGMiC0Du8nUJAAAAAElFTkSuQmCC" 5 3 3
   "0a14053214055a1405821405aa14050a5a05325a235a5a41825a5faa5a7d0aa00532a0415aa07d82a0b9aaa0f5")
  (rgb-8-trns "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADCAIAAADUVFKvAAAABnRSTlMAWgAUAAV3QC/YAAAAMElEQVR42mPgEmE1EmGNEmFtEmFdJcLKyBXFqsEgB0dMDG4MDG5yDG42DG5RDG4VAIv4Bk7/RHExAAAAAElFTkSuQmCC" 5 3 4
   "0a1405ff321405ff5a140500821405ffaa1405ff0a5a05ff325a23ff5a5a41ff825a5fffaa5a7dff0aa005ff32a041ff5aa07dff82a0b9ffaaa0f5ff")
  (palette-1 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADAQMAAABh+Fe7AAAABlBMVEUAAAANOVtfgi9aAAAADklEQVR42mMIYFzBtAIABD4BpCPmtzEAAAAASUVORK5CYII=" 5 3 3
   "0000000d395b0000000d395b0000000d395b0000000d395b0000000d395b0000000d395b0000000d395b000000")
  (palette-2-trns "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADAgMAAAAmWC1rAAAADFBMVEUAAAANOVsacrYnqxF88bN7AAAAA3RSTlMAgMhUShYXAAAAEUlEQVR42mOwZGD0+8Tk6gAAB/UCAmjO8aQAAAAASUVORK5CYII=" 5 3 4
   "0000000027ab11ff1a72b6c80d395b80000000000d395b800000000027ab11ff1a72b6c80d395b801a72b6c80d395b800000000027ab11ff1a72b6c8")
  (palette-4 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADBAMAAACpGNjLAAAAMFBMVEUAAAANOVsacrYnqxE05GxBHcdOViJbj31oyNh1ATOCOo6Pc+mcrESp5Z+2HvrDV1UYW4LCAAAAFElEQVR42mNgzjzAKJIWxiQoKAAAEBoCMsexgMQAAAAASUVORK5CYII=" 5 3 3
   "00000027ab114e56227501339cac440d395b34e46c5b8f7d823a8ea9e59f1a72b6411dc768c8d88f73e9b61efa")
  (palette-8-trns "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADCAMAAABs6DXKAAAAPFBMVEUAAAANOVsacrYnqxE05GxBHcdOViJbj31oyNh1ATOCOo6Pc+mcrESp5Z+2HvrDV1XQkLDdyQvqAmb3O8GE2MtxAAAAA3RSTlMAgMhUShYXAAAAFklEQVR42mNgYGbj5GFkZAYCJkYQAAACTgA0phKdVQAAAABJRU5ErkJggg==" 5 3 4
   "0000000027ab11ff4e5622ff750133ff9cac44ff0d395b8034e46cff5b8f7dff823a8effa9e59fff1a72b6c8411dc7ff68c8d8ff8f73e9ffb61efaff")
  (gray-alpha-8 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADCAQAAADxPw1zAAAAHklEQVR42mPg1TCKCu+p2bfwA2OGm6oRBDJFy8EgAKN5CNpugrDlAAAAAElFTkSuQmCC" 5 3 4
   "0d0d0d283232325a5757578c7c7c7cbea1a1a1f0686868468d8d8d78b2b2b2aad7d7d7dcfcfcfc0ec3c3c364e8e8e8960d0d0dc8323232fa5757572c")
  (gray-alpha-16 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADEAQAAAChr9EwAAAALklEQVR42mPgZdBgMOKOYg0X6+GqUdzHv1Dngwhjhqwbpyq3ESsyZoqWleNExwBvVAqetReqHwAAAABJRU5ErkJggg==" 5 3 4
   "0d0d0d283232325a5757578c7c7c7cbea1a1a1f0686868468d8d8d78b2b2b2aad7d7d7dcfcfcfc0ec3c3c364e8e8e8960d0d0dc8323232fa5757572c")
  (rgba-8 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADCAYAAABbNsX4AAAAOElEQVR42mPgEmHVMBJhjYoSYe1pEmHdt0qE9QMjVxSrmwaDnBEyZmJwY5BjcJMDYhsgjgLiCjkAY7sKrp9t9/QAAAAASUVORK5CYII=" 5 3 4
   "0a1405283214055a5a14058c821405beaa1405f00a5a0546325a23785a5a41aa825a5fdcaa5a7d0e0aa0056432a041965aa07dc882a0b9faaaa0f52c")
  (rgba-16 "iVBORw0KGgoAAAANSUhEUgAAAAUAAAADEAYAAAALphm7AAAAWklEQVR42mPgYhQxZk3VYDDiELFizYlijeIXcWQt7uFqEhPxYK3ax79KVsSftfGDCCMXS5QZa4YbpwY7A7scuxErLpqJgdmNmYFZjhNEy0FpGygdBaUrmOU4AZu7EBxhn7Q6AAAAAElFTkSuQmCC" 5 3 4
   "0a1405283214055a5a14058c821405beaa1405f00a5a0546325a23785a5a41aa825a5fdcaa5a7d0e0aa0056432a041965aa07dc882a0b9faaaa0f52c")
  (adam7-rgba-8 "iVBORw0KGgoAAAANSUhEUgAAAAoAAAAFCAYAAAELY03+AAAAtElEQVR42mPgEmHV8BJh3cG4SoT1AyOXDuuCBQwPToAwc5QIa89ZLmY+FgaJDxUMQBFGrgWsKQEMFSiYwUiENapJhHXfJRFWJSUR1rYiEdZXjEYLHKehq2Ri6LGxYejZAsQ6QLwEiGVsmLiiWN2MopQroqIcVzVFxd9ZFVXLdylqtsOvqJ1FSlHXl3hFfb1WFCXMwcy6kzlexM3bXMQtE4jbgVgViJ2BONF8itt/IC0LxLvNAc3TQEk3Hky+AAAAAElFTkSuQmCC" 10 5 4
   "0a1405283214055a5a14058c821405beaa1405f0d2140522fa140554221405864a1405b8721405ea0a5a0546325a23785a5a41aa825a5fdcaa5a7d0ed25a9b40fa5ab972225ad7a44a5af5d6725a13080aa0056432a041965aa07dc882a0b9faaaa0f52cd2a0315efaa06d9022a0a9c24aa0e5f472a021260ae6058232e65fb45ae6b9e682e61318aae66d4ad2e6c77cfae621ae22e67be04ae6d51272e62f440a2c05a0322c7dd25a2cf504822c6d36aa2ce568d22c5d9afa2cd5cc222c4dfe4a2cc530722c3d62")
  (adam7-gray-1 "iVBORw0KGgoAAAANSUhEUgAAAAMAAAACAQAAAAHCCGshAAAAEElEQVR42mNgYGZgaGB6AAACfgFmiCpfmwAAAABJRU5ErkJggg==" 3 2 1
   "00ff00ffffff")
  (adam7-palette-4-trns "iVBORw0KGgoAAAANSUhEUgAAAAoAAAAHBAMAAAG0hfHGAAAAMFBMVEUAAAANOVsacrYnqxE05GxBHcdOViJbj31oyNh1ATOCOo6Pc+mcrESp5Z+2HvrDV1UYW4LCAAAAA3RSTlMAgMhUShYXAAAAPklEQVR42mPgYDzA6NDAnMTiwqixZw+Ti4sDg+XXDYzRe3YyKSkpMCe+z2YSqbpgNodZx8XmiAuLkpJSmhAAkd8PQXROAi0AAAAASUVORK5CYII=" 10 7 4
   "0000000027ab11ff4e5622ff750133ff9cac44ffc35755ff1a72b6c8411dc7ff68c8d8ff8f73e9ff0d395b8034e46cff5b8f7dff823a8effa9e59fff0000000027ab11ff4e5622ff750133ff9cac44ff1a72b6c8411dc7ff68c8d8ff8f73e9ffb61efaff0d395b8034e46cff5b8f7dff823a8effa9e59fff27ab11ff4e5622ff750133ff9cac44ffc35755ff1a72b6c8411dc7ff68c8d8ff8f73e9ffb61efaff34e46cff5b8f7dff823a8effa9e59fff0000000027ab11ff4e5622ff750133ff9cac44ffc35755ff411dc7ff68c8d8ff8f73e9ffb61efaff0d395b8034e46cff5b8f7dff823a8effa9e59fff000000004e5622ff750133ff9cac44ffc35755ff1a72b6c8411dc7ff68c8d8ff8f73e9ffb61efaff0d395b80")
  (adam7-gray-alpha-16 "iVBORw0KGgoAAAANSUhEUgAAAAkAAAADEAQAAAHMlAEoAAAAaUlEQVR42mPgZdBgMI3YocG4UOeDCHO4WA/XAXM+ScbDVilCXmIpXMiYwYg7irVGcR//MXMlSQHfNmXGF67TxJFVMGXIunH2alTwbTJeJXzd7o7EH08+WcUQByW3+CL17KwlOhNKrxkCAHr8H8C4+FRGAAAAAElFTkSuQmCC" 9 3 4
   "0d0d0d283232325a5757578c7c7c7cbea1a1a1f0c6c6c622ebebeb5410101086353535b8686868468d8d8d78b2b2b2aad7d7d7dcfcfcfc0e21212140464646726b6b6ba4909090d6c3c3c364e8e8e8960d0d0dc8323232fa5757572c7c7c7c5ea1a1a190c6c6c6c2ebebebf4"))

(deftest png-writer-reads-back-as-its-own-pixels ()
  (dolist (channels '(1 3 4))
    (let* ((pixels (test-pixels 7 5 channels (lambda (x y k) (mod (+ (* x 37) (* y 11) (* k 90) (* x y k)) 256))))
           (back (nle::png-pixels (nle::pixels-png pixels))))
      (is (= channels (nle::pixels-channels back)))
      (is (equalp (nle::pixels-data pixels) (nle::pixels-data back)))))
  ;; The checksums: a 1 x 1 RGBA picture's header and end chunks are, CRC
  ;; and all, byte for byte the ones another encoder wrote into
  ;; *ONE-PIXEL-PNG*.
  (let ((png (nle::pixels-png (test-pixels 1 1 4 (constantly 0))))
        (end (- (length *one-pixel-png*) 12)))
    (is (equalp (subseq *one-pixel-png* 0 33) (subseq png 0 33)) "the signature and IHDR")
    (is (equalp (subseq *one-pixel-png* end) (subseq png (- (length png) 12))) "and IEND")))

;;; --- F2: JPEG ---------------------------------------------------------------------

(deftest jpeg-decodes-red-first-and-gray-as-one-channel ()
  (let ((colour (nle::jpeg-pixels (fit-fixture 'jpeg-colour)))
        (gray (nle::jpeg-pixels (fit-fixture 'jpeg-gray))))
    (is-shape colour (nle::pixels-width 16) (nle::pixels-height 8) (nle::pixels-channels 3))
    (is (near '(230 30 30) (pixel-at colour 2 3)) "the left half is red")
    (is (near '(30 30 230) (pixel-at colour 13 4)) "the right half is blue")
    (is (= 1 (nle::pixels-channels gray)))
    (is (near '(60) (pixel-at gray 2 3)))
    (is (near '(200) (pixel-at gray 13 4))))
  (let ((progressive (nle::jpeg-pixels (fit-fixture 'jpeg-progressive))))
    (is-shape progressive (nle::pixels-width 16) (nle::pixels-height 8) (nle::pixels-channels 3))
    (is (near '(230 30 30) (pixel-at progressive 2 3)) "a progressive JPEG decodes too: red")
    (is (near '(30 30 230) (pixel-at progressive 13 4)) "and blue"))
  (is (search "does not decode here"
              (refusal-text nle::image-unfit (nle::jpeg-pixels (fit-fixture 'jpeg-arithmetic))))))

(deftest jpeg-orientation-is-read-in-either-byte-order ()
  (is (= 6 (nle::jpeg-orientation (fit-fixture 'jpeg-exif-6-intel))))
  (is (= 8 (nle::jpeg-orientation (fit-fixture 'jpeg-exif-8-motorola))))
  (is (= 1 (nle::jpeg-orientation (fit-fixture 'jpeg-colour))) "a JPEG naming none stands as stored"))

;;; 1 2 3 over 4 5 6, turned upright by each orientation EXIF has.
(deftests orient-pixels-turns (orientation rows)
  (let ((turned (nle::orient-pixels (test-pixels 3 2 1 (lambda (x y k) (declare (ignore k)) (+ 1 x (* 3 y))))
                                    orientation)))
    (is (equal rows (loop for y below (nle::pixels-height turned)
                          collect (loop for x below (nle::pixels-width turned)
                                        collect (first (pixel-at turned x y)))))))
  (upright 1 '((1 2 3) (4 5 6)))
  (mirrored 2 '((3 2 1) (6 5 4)))
  (half-way 3 '((6 5 4) (3 2 1)))
  (flipped 4 '((4 5 6) (1 2 3)))
  (transposed 5 '((1 4) (2 5) (3 6)))
  (a-quarter-clockwise 6 '((4 1) (5 2) (6 3)))
  (transversed 7 '((6 3) (5 2) (4 1)))
  (a-quarter-anticlockwise 8 '((3 6) (2 5) (1 4))))

;;; --- F3: shrinking ----------------------------------------------------------------

(deftest shrink-is-by-area-with-alpha-premultiplied ()
  (let ((row (test-pixels 3 1 1 (lambda (x y k) (declare (ignore y k)) (nth x '(0 90 255))))))
    (is (equalp #(30 200) (nle::pixels-data (nle::shrink-pixels row 2 1))) "each pixel by the part it covers"))
  ;; An opaque red beside a transparent green: the green has no say in the
  ;; colour, only in the alpha.
  (let ((pair (test-pixels 2 1 4 (lambda (x y k) (declare (ignore y)) (nth k (if (zerop x) '(255 0 0 255) '(0 255 0 0)))))))
    (is (equalp #(255 0 0 128) (nle::pixels-data (nle::shrink-pixels pair 1 1)))))
  (is (equal '(2000 1500) (multiple-value-list (nle::fit-size 4000 3000 2000))))
  (is (equal '(800 600) (multiple-value-list (nle::fit-size 800 600 2000))))
  (is (equal '(2000 1) (multiple-value-list (nle::fit-size 9000 1 2000))) "never shrunk to nothing"))

;;; --- F4: fitting ------------------------------------------------------------------

(deftest fit-image-sends-what-fits-as-itself ()
  (is-values (bytes media-type width height) (nle::fit-image *one-pixel-png*)
    (bytes eq *one-pixel-png* "the very bytes") (media-type "image/png") (width 1) (height 1)))

(deftest fit-image-sends-a-photo-as-jpeg-and-a-diagram-as-png ()
  (is-values (bytes media-type width height) (nle::fit-image (nle::pixels-png (photo-pixels 64 48)) :max-side 32)
    (media-type "image/jpeg" "a photo goes as the smaller JPEG") (width 32) (height 24)
    ((multiple-value-list (nlt:image-dimensions bytes)) '(32 24)))
  (is-values (bytes media-type width height) (nle::fit-image (nle::pixels-png (flat-pixels 64 48)) :max-side 32)
    (media-type "image/png" "a flat picture as the smaller PNG") (width 32) (height 24)
    ((nle::pixels-data (nle::png-pixels bytes)) equalp (nle::pixels-data (flat-pixels 32 24)))))

(deftest fit-image-turns-a-photo-upright-by-its-exif-orientation ()
  ;; Red left, blue right, 16 x 8, shrunk to 8 x 4 and then turned: a
  ;; quarter clockwise puts the red on top, a quarter anticlockwise the blue.
  (is-values (bytes media-type width height) (nle::fit-image (fit-fixture 'jpeg-exif-6-intel) :max-side 8)
    (width 4) (height 8)
    ((near '(230 30 30) (pixel-at (nle::image-pixels bytes media-type) 2 1)) is "red on top")
    ((near '(30 30 230) (pixel-at (nle::image-pixels bytes media-type) 2 6)) is "blue below"))
  (is-values (bytes media-type width height) (nle::fit-image (fit-fixture 'jpeg-exif-8-motorola) :max-side 8)
    (width 4) (height 8)
    ((near '(30 30 230) (pixel-at (nle::image-pixels bytes media-type) 2 1)) is "blue on top")))

(deftest fit-image-trades-quality-and-then-size-for-bytes ()
  (let* ((photo (photo-pixels 40 40))
         (png (nle::pixels-png photo))
         (limit (length (nle::pixels-jpeg photo 55))))
    (is-values (bytes media-type width) (nle::fit-image png :max-bytes limit)
      (bytes equalp (nle::pixels-jpeg photo 55) "the first quality that fits, at full size")
      (media-type "image/jpeg") (width 40))
    (let ((limit (floor (length (nle::pixels-jpeg photo 40)) 2)))
      (is-values (bytes media-type width height) (nle::fit-image png :max-bytes limit)
        ((<= (length bytes) limit) is "under the bytes") ((< width 40) is "by shrinking")
        (height width "the shape kept")
        ((nle::pixels-width (nle::image-pixels bytes media-type)) = width "opening at the size it says")))))

(deftest fit-image-refuses-what-cannot-fit ()
  (is (search "not even one pixel"
              (refusal-text nle::image-unfit
                (nle::fit-image (nle::pixels-png (photo-pixels 8 8)) :max-side 4 :max-bytes 10))))
  ;; A header claiming 30000 x 30000 over one pixel's data: refused before
  ;; any of it is inflated.
  (let* ((bomb (replace (copy-seq *one-pixel-png*) (octets 0 0 117 48 0 0 117 48) :start1 16))
         (reason (refusal-text nle::image-unfit (nle::fit-image bomb))))
    (is (search "30,000x30,000" reason) reason)
    (is (search "pixels one image is opened at" reason) reason))
  (is (search "open no PNG, JPEG, GIF or WebP" (refusal-text nle::image-unfit (nle::fit-image (octets 1 2 3 4))))))

(deftest fit-image-sends-what-it-cannot-open-whole-or-through-a-transcoder ()
  (let* ((arithmetic (fit-fixture 'jpeg-arithmetic))
         ;; the same, its frame header claiming 9000 wide
         (wide (replace (copy-seq arithmetic) (octets #x23 #x28)
                        :start1 (+ 7 (search (octets #xFF #xC9) arithmetic)))))
    (let ((nlt:*png-transcoders* '()))
      (is-values (bytes media-type width height) (nle::fit-image arithmetic :max-side 8)
        (bytes eq arithmetic "within what a provider takes whole") (media-type "image/jpeg") (width 16) (height 8))
      ;; 9000 wide, past what a provider takes whole.
      (let ((reason (refusal-text nle::image-unfit (nle::fit-image wide))))
        (is (search "no ImageMagick, ffmpeg or sips here opens it" reason) reason)))
    ;; With a transcoder, the PNG it writes is what is fitted: here one that
    ;; answers any file with a 16 x 8 picture.
    (let ((picture (namestring (temp-path "fit-transcoded" "png"))))
      (nlk:with-cleanup ((ignore-errors (delete-file picture)))
        (alexandria:write-byte-vector-into-file (nle::pixels-png (flat-pixels 16 8)) picture)
        (let ((nlt:*png-transcoders* `(("cp" ,picture :target))))
          (is-values (bytes media-type width height) (nle::fit-image arithmetic :max-side 8)
            (media-type "image/png") (width 8) (height 4)
            ((multiple-value-list (nlt:image-dimensions bytes)) '(8 4))))))))

;;; --- F5: the doors ----------------------------------------------------------------

(defun turn-image (turn-id)
  "The first image staged on TURN-ID, (MEDIA-TYPE . BASE64)."
  (first (cdr (first (nle::turn-images turn-id)))))

(deftest look-sends-a-large-image-shrunk-and-says-so ()
  (with-look-image (path "look-wide" :turn-id "look-wide-turn" :call "call-wide"
                         :octets (nle::pixels-png (flat-pixels 2400 10)))
    (nle::forget-turn-images "look-wide-turn")
    (let ((receipt (nle:look path))
          (staged (turn-image "look-wide-turn")))
      (is (search "image/png 2400x10" receipt) receipt)
      (is (search "sent shrunk to image/png 2000x8" receipt) receipt)
      (is (equal "image/png" (car staged)))
      (is (equal '(2000 8) (multiple-value-list (nlt:image-dimensions (base64-octets (cdr staged)))))))))

(deftest look-refuses-an-image-it-cannot-fit ()
  (with-look-image (path "look-bomb" :turn-id "look-bomb-turn" :call "call-bomb"
                         :octets (replace (copy-seq *one-pixel-png*) (octets 0 0 117 48 0 0 117 48) :start1 16))
    (nle::forget-turn-images "look-bomb-turn")
    (let ((report (refusal-text error (nle:look path))))
      (is (search "cannot be shown" report) report)
      (is (search "pixels one image is opened at" report) report)
      (is (null (nle::turn-images "look-bomb-turn")) "and stages nothing")))
  (with-look-image (path "look-heavy" :turn-id "look-heavy-turn" :call "call-heavy")
    (let* ((nle::*image-read-max-bytes* 64)
           (report (refusal-text error (nle:look path))))
      (is (search "70 bytes, over the 64-byte ceiling" report) report))))

(deftest every-image-a-turn-stages-is-fitted-first ()
  (let ((wide (cl-base64:usb8-array-to-base64-string (nle::pixels-png (flat-pixels 2400 10))))
        (small (cl-base64:usb8-array-to-base64-string *one-pixel-png*)))
    (nlk:with-cleanup ((nle::forget-turn-images "fit-turn"))
      (nle::attach-turn-images "fit-turn" (list (cons "image/png" wide) (cons "image/png" small)
                                                (cons "image/png" "QUJD")))
      (destructuring-bind ((anchor fitted kept unread)) (nle::turn-images "fit-turn")
        (is (null anchor) "an input's own pictures")
        (is (equal '(2000 8) (multiple-value-list (nlt:image-dimensions (base64-octets (cdr fitted))))))
        (is (eq small (cdr kept)) "one within both ceilings is staged as it came")
        (is (equal "QUJD" (cdr unread)) "bytes nothing opens go as they are")))))
