(in-package :utils)

(defun uint8p (object)
  (typep object '(unsigned-byte 8)))
(export 'uint8p)

(defun int8p (object)
  (typep object '(signed-byte 8)))
(export 'int8p)

(defun uint12p (object)
  (typep object '(unsigned-byte 12)))
(export 'uint12p)

(defun int12p (object)
  (typep object '(signed-byte 12)))
(export 'int12p)

(defun uint16p (object)
  (typep object '(unsigned-byte 16)))
(export 'uint16p)

(defun int16p (object)
  (typep object '(signed-byte 16)))
(export 'int16p)

(defun uint24p (object)
  (typep object '(unsigned-byte 24)))
(export 'uint24p)

(defun int24p (object)
  (typep object '(signed-byte 24)))
(export 'int24p)

(defun uint32p (object)
  (typep object '(unsigned-byte 32)))
(export 'uint32p)

(defun int32p (object)
  (typep object '(signed-byte 32)))
(export 'int32p)

(defun uint64p (object)
  (typep object '(unsigned-byte 64)))
(export 'uint64p)

(defun int64p (object)
  (typep object '(signed-byte 64)))
(export 'int64p)
