(defun http-char (c1 c2 &optional (default #\space))
  (let
    ((code (parse-integer (coerce (list c1 c2) 'string) :radix 16 :junk-allowed t))) ;interpret c1c2 as hex number
    (if code (code-char code) default)))

(defun decode-param (s)
  (labels 
    ((f (lst)
      (when lst
        (case (car lst)
          (#\% (cons (http-char (cadr lst) (caddr lst)) (f (cdddr lst)))) ;2 digits after '%'
          (#\+ (cons #\space (f (cdr lst))))
          (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s) ; generate alist from str of request parameter
  (let
    ((i1 (position #\= s))
     (i2 (position #\& s)))
    (cond 
      (i1 (cons (cons (intern (string-upcase (subseq s 0 i1))) (decode-param (subseq s (1+ i1) i2))) (and i2 (parse-params (subseq s (1+ i2)))))) ;include '='?
      ;hoge=h&fuga=fをparseしてalistに
      ((equal s "") nil)
      (t s))))

;この簡単な例ではGET/POSTの区別をつけてない.
(defun parse-url (s) ;analyze 1st line of request header(ex. GET /lolcats.html HTTP/1.1)
  (let* 
    ((url (subseq s (+ 2 (position #\space s)) (position #\space s :from-end t))) 
     (x (position #\? url)))
    (if x
      (cons (subseq url 0 x) (parse-params (subseq url (1+ x)))) ;url?request parameter
      (cons url '()))))

(defun get-header (stream) ; analyze "hoge: aaaaa"
  (let*
    ((s (read-line stream))
     (h 
       (let 
         ((i (position #\: s)))
         (when i
           (cons (intern (string-upcase (subseq s 0 i))) (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream))))) ;1 set per 1 line

(defun get-content-params (stream header) ;analyze requeset body
  (let 
    ((length) (cdr (assoc 'content-length header)))
    (when length
      (let 
        ((content (make-string (parse-integer length)))) ;make-string makes string variable of n letters
        (read-sequence content stream)
        (parse-params content))))) ;interpret content -> alist

(defun serve (requset-handler) ;implemention of the server
  (let
    ((socket (socket-server 8080))) ;make socket
    (unwind-protect
      (loop 
        (with-open-stream (stream (socket-accept socket)) ;make&open stream from socket
          (let*
            ((url (parse-url (read-line stream)))
             (path (car url))
             (header (get-header stream))
             (params (append (cdr url) (get-content-params stream header)))
             (*standard-output* stream)) ; standard output goes to stream
            (funcall requset-handler path header params)))) ;requeset-handler is a function generating the content of the Website in HTML
      (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (if (equal path "greeting") ; content of "/greeting"
    (let
      ((name (assoc 'name params)))
      (if (not name)
        (let
          ((content "<html><body><form> What is your name?<input name='name' /> </form></body></html>"))
          (princ (add-response-header content 200)))
        (let
          ((content (format nil "<html>Nice to meet you, ~a!</html>" (cdr name))))
          (princ (add-response-header content 200)))))
    (let
      ((content "<html>SORRY... THAT PAGE NOT FOUND.</html>"))
      (princ (add-response-header content 404)))))

(defun add-response-header (content code-num)
  (let
    ((response-header 
       (cond
        ((eq code-num 404) (format nil "HTTP/1.1 ~d Not Found" code-num)) ;404
        (t (format nil "HTTP/1.1 ~d OK" code-num)))) ;200
     (blank (format nil "~%~%")))
    (concatenate 'string response-header blank content)))

(serve #'hello-request-handler)