(define-module (file util)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-13)
  #:export(current-directory home-directory temporary-directory directory-list build-path absolute-path? relative-path?
                             expand-path decompose-path path-extension))
;;
;; ドキュメント文字列の内容は Gauche の file.util からとってます。
;;
(define* (current-directory #:optional (new-director #f))
  "引数無しで呼ばれた場合、カレントディレクトリを返します。
文字列 ~new-directory~ が与えられた場合はプロセスのカレントディレクトリを ~new-directory~ に変更します。
変更ができなかった場合はエラーとなります。"
  (getcwd))

(define* (home-directory #:optional user)
  "名前または整数のユーザーIDで与えられたユーザー ~user~ のホームディレクトリを返します。
~user~ が省略された場合はカレントユーザーが使われます。
与えられたユーザーが見つけられないか、ホームディレクトリを決定できなかった場合は ~#f~ が返されます。"
  (let ((home-dir (getenv "HOME")))
    (if home-dir
        home-dir
        (passwd:dir (getpwuid (getuid))))))

(define (temporary-directory)
  "一時ファイルを作るのに適したディレクトリ名を保持しているパラメータらしいのですが、
ここでは手続きにしています。
それで、どうやら guile の POSIX には ~tmpdir~ が無いようなので環境変数 TMPDIR と TMP がこの順でチェックされ、
フォールバックとして /tmp が返されるということにします。"
  (let ((tmp-dir (getenv "TMPDIR")))
    (if tmp-dir
        (begin
          (set! tmp-dir (getenv "TMP"))
          (if tmp-dir
              tmp-dir
              "/tmp"))
        tmp-dir)))

(define* (directory-list path #:key children? add-path? filter filter-add-path?)
  "ディレクトリ path 中のエントリのリストを返します。
リストは文字列順にソートされます。

デフォルトではエントリのベースネーム(パスの最後のコンポーネント)のみが返されますが、
キーワード引数 add-path? に真の値が与えられた時は path が各エントリの前に追加されます。
children? に真の値が与えられた時は、カレントディレクトリと親ディレクトリがリストから除かれます。

引数 filter は、もし与えられれば 1 つの引数を取る手続きでなければなりません。
ディレクトリ中の各エントリを引数としてその手続きが呼ばれ、真を返したエントリのみが結果に含まれます。
filter に与えられるエントリはデフォルトではベース名のみですが、
引数 filter-add-path? が真ならば path が前に追加された名前となります。

path がディレクトリでない場合はエラーが報告されます。"
  (unless (file-exists? path)
          (throw 'system-error (format "could'n open directory \"~a\"" path)))
  (letrec ((recur (lambda (results directory-stream)
                    (let* ((component (readdir directory-stream))
                           (component-path (if add-path?
                                               (build-path path component)
                                               component)))
                      (if (eof-object? component)
                          results
                          (recur (cons component-path
                                       results)
                                 directory-stream))))))
    (let ((results (sort (recur '() (opendir path)) string<?)))
      (if children?
          (filter (lambda (x)
                    (not (or (string-suffix? "."  x)
                             (string-suffix? ".." x))))
                  results)
          results))))

(define* (directory-fold path proc seed #:key lister follow-link?)
  "ディレクトリ探索の最も基本的な手続きです。
基本的な動作は以下に示すような再帰的なものです。

- ~path~ がディレクトリでない場合は ~(proc path seed)~ を評価し、結果を返します。
- ~path~ がディレクトリであった場合、まず ~(lister path seed)~ を評価します。
手続き ~lister~ は 2 つの値、パス名のリストと次の ~seed~ となる値を返さなければなりません。
続いて、 ~directory-fold~ が各パス名に対して再帰的に呼ばれます。
各呼び出しの結果が次の再帰呼出しの ~seed~ の値に使われます。

デフォルトの ~lister~ は ~directory-list~ を次のように呼び出すものです。

#+begin_src scheme
  (lambda (path seed)
    (values (directory-list path :add-path? #t :children? #t)
            seed))
#+end_src"
  (unless lister
          (set! lister (lambda (path seed)
                         (values (directory-list path :add-path? #t :children? #t)
                                 seed))))
  (if (file-is-directory? path)
      (let-values ((paths next-seed) (lister path seed))
        (letrec ((recur (lambda (paths seed)
                          (recur (cdr paths)
                                 (directory-fold (car paths)
                                                 proc
                                                 seed
                                                 #:lister lister
                                                 #:follow-link? follow-link?)))))
          (recur paths next-seed)))
      (proc path seed)))

(define* (build-path base-path #:rest components)
  "パス名のコンポーネント component を base-path に追加します。"
  (string-join (map (lambda (path)
                      (string-trim-both path (string-ref file-name-separator-string 0)))
                    (cons base-path components))
               file-name-separator-string
               'prefix))

(define absolute-path? absolute-file-name?)

(define (relative-path? path)
  "path が相対パスならば、#t を返します。"
  (or (string-prefix? "." path)
      (string-prefix? ".." path)
      (not (string-prefix? "/" path))))

(define (expand-path path)
  "path がチルダ表示を含んでいたらそれを展開したものを返します。
そうでなければ path そのものを返します。
この手続きは path が存在し、アクセス可能であるかどうかはチェックしません。"
  (if (string-prefix? "~" path)
      (let ((home-dir (home-directory)))
        (string-append home-dir (string-drop path 1)))
      path))

(define (decompose-path path)
  "パス名 path のディレクトリ部、拡張子を除いたファイル名、そして拡張子の 3 つの値を返します。
パス名が拡張子を持たない場合、最後の値は #f になります。
パス名がディレクトリセパレータで終わっている場合は 2 番目と 3 番目(1 based)の値が #f になります。"
  (let* ((dir-path (dirname path))
         (extention (path-extension path))
         (base-filename (if extention
                            (basename path (string-append "." extention))
                            #f)))
    (values dir-path base-filename extention)))

(define (path-extension path)
  "path の拡張子を返します。
path が拡張子を持っていない場合は #f が返されます。"
  (let ((found-index (string-index-right path #\.)))
    (if found-index
        (string-copy path (+ found-index 1))
        #f)))
