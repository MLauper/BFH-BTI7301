;(open "/tmp/TestLispWrite.txt" :direction :output :if-exists :supersede)

(with-open-file (str "/tmp/testFile.txt"
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
  (format str "This is some Output"))

