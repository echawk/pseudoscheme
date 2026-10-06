;;; Tests for SRFI 109: the SRFI's examples.
(import (scheme base) (scheme char) (scheme process-context) (srfi 64) (srfi 109))
(test-begin "srfi-109")

(test-equal "ONE TWO THREE\nUNO DOS TRES\n" (string-upcase &{one two three
uno dos tres
}))
(define name "World")
(test-equal "Hello World!" &{Hello &[name]!})
(test-equal "Hello WORLD!" &{Hello &(string-upcase name)!})
(test-equal "a\nb" &{a&newline;b})
(test-equal "abc  def" &{abc&-
  def})
(test-equal "ONE TWO THREE\nUNO DOS TRES\n" (string-upcase &{
     &|one two three
     &|uno dos tres
}))
(test-equal "This is the first of 2 lines.\nThis last line is followed by a final newline.\n" &{
   &|This is the first of 2 lines.
   &|This last line is followed by a final newline.
})
(test-equal "first\nlast" &{
   &|first
   &|last})
(test-equal "preamble  postamble" &{preamble &#|ignore this part|# postamble})
(test-equal (string #\escape #\escape) &{&#27;&#x1B;})
(test-equal "& < > \" '" &{&amp; &lt; &gt; &quot; &apos;})
(test-equal "}_{" &{&rbrace;_&lbrace;})
(test-equal "A left brace '{' followed by a right brace '}' is ok."
  &{A left brace '{' followed by a right brace '}' is ok.})
(test-equal "Lærdalsøyri" &{L&aelig;rdals&oslash;yri})
(define $entity$:crnl "\r\n")
(test-equal "\r\n" &{&crnl;})
(test-equal '($string$ "a" $<<$ x $>>$ "b") '&{a&[x]b})
(test-equal "1 2" &{&[1] &[2]})

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-109")
  (exit (if (zero? failures) 0 1)))
