;;; ox-reactbook.el --- Reactbook.css Back-End for Org Export Engine -*- lexical-binding: t -*-

;;; Code:

;;; Dependencies:
(require 'org)
(require 'ox-html)
(require 's)
(require 'dash)
(require 'cl)

;;; Define backend
(org-export-define-backend 'reactbook
  '((bold . org-reactbook-bold)
    (center-block . org-html-center-block)
    (clock . org-html-clock)
    (code . org-reactbook-code)
    (drawer . org-html-drawer)
    (dynamic-block . org-html-dynamic-block)
    (entity . org-html-entity)
    (example-block . org-reactbook-example-block)
    (export-block . org-html-export-block)
    (export-snippet . org-html-export-snippet)
    (fixed-width . org-html-fixed-width)
    (footnote-reference . org-html-footnote-reference)
    (headline . org-reactbook-headline)
    (horizontal-rule . org-reactbook-horizontal-rule)
    (inline-src-block . org-html-inline-src-block)
    (inlinetask . org-html-inlinetask)
    (inner-template . org-reactbook-inner-template)
    (italic . org-reactbook-italic)
    (item . org-reactbook-item)
    (keyword . org-html-keyword)
    (latex-environment . org-html-latex-environment)
    (latex-fragment . org-html-latex-fragment)
    (line-break . org-html-line-break)
    (link . org-reactbook-link)
    (node-property . org-html-node-property)
    (paragraph . org-reactbook-paragraph)
    (plain-list . org-reactbook-plain-list)
    (plain-text . org-reactbook-plain-text)
    (planning . org-html-planning)
    (property-drawer . org-html-property-drawer)
    (quote-block . org-reactbook-blockquote)
    (radio-target . org-html-radio-target)
    (section . org-reactbook-section)
    (special-block . org-reactbook-special-block)
    (src-block . org-reactbook-src-block-select)
    (statistics-cookie . org-html-statistics-cookie)
    (strike-through . org-reactbook-strike-through)
    (subscript . org-html-subscript)
    (superscript . org-html-superscript)
    (table . org-reactbook-table)
    (table-cell . org-reactbook-table-cell)
    (table-row . org-reactbook-table-row)
    (target . org-reactbook-target)
    (template . org-reactbook-template)
    (timestamp . org-html-timestamp)
    (underline . org-reactbook-underlined)
    (verbatim . org-reactbook-verbatim)
    (verse-block . org-html-verse-block))
  :menu-entry
  '(?x "Export to HTML with org-reactbook Back-End"
       ((?H "As HTML buffer" org-reactbook-export-as-html)
        (?h "As HTML file" org-reactbook-export-to-html)
        (?o "As HTML file and open"
            (lambda (a s v b)
              (if a (org-reactbook-export-to-html t s v b)
                (org-open-file (org-reactbook-export-to-html nil s v b)))))))
  :options-alist
  '((:html-footnote-format nil nil org-reactbook-class-footnotes-format)
    (:html-footnote-separator nil nil org-reactbook-class-footnotes-separator)
    (:html-footnotes-section nil nil org-reactbook-class-footnotes-section)))

;;; reactbook group

(defgroup org-reactbook nil
  "Classes for the html elements."
  :group 'classes)


;;; Element Classes

;; Headings

(defcustom org-reactbook-class-title
  "mt-12 mb-12 text-4xl text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Reactbook.css classes for the Title."
  :type '(string))

(defcustom org-reactbook-class-h1
  "mt-24 mb-6 text-3xl text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Reactbook.css classes for Heading 1."
  :type '(string))

(defcustom org-reactbook-class-h2
  "mt-20 mb-6 text-2xl text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Reactbook.css classes for Heading 2."
  :type '(string))

(defcustom org-reactbook-class-h3
  "mt-16 mb-6 text-xl text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Reactbook.css classes for Heading 3."
  :type '(string))

(defcustom org-reactbook-class-h4
  "mt-14 mb-6 text-lg text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Reactbook.css classes for Heading 4."
  :type '(string))

(defcustom org-reactbook-class-h5
  "mt-12 mb-6 text-lg text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Reactbook.css classes for Heading 5."
  :type '(string))

(defcustom org-reactbook-class-h6
  "mt-10 mb-6 text-base text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Reactbook.css classes for Heading 6."
  :type '(string))

(defcustom org-reactbook-class-h7
  "mt-8 mb-6 text-base text-gray-700 dark:text-gray-400 border-b \
hover:text-blue-400 dark:hover:text-blue-500 border-gray-500"
  "Reactbook.css classes for Heading 7."
  :type '(string))


;; Text elements

(defcustom org-reactbook-class-bold
  "font-bold"
  "Reactbook.css classes for the HTML BOLD attribute."
  :type '(string))

(defcustom org-reactbook-class-italic
  "italic"
  "Reactbook.css classes for the HTML ITALIC attribute."
  :type '(string))

(defcustom org-reactbook-class-underlined
  "underline"
  "Reactbook.css classes for the HTML UNDERLINE attribute."
  :type '(string))

(defcustom org-reactbook-class-code
  "px-2 rounded text-green-500 bg-gray-200 dark:bg-gray-600 \
dark:text-yellow-500"
  "Reactbook.css classes for the HTML UNDERLINE attribute."
  :type '(string))

(defcustom org-reactbook-class-verbatim
  "px-2 rounded text-red-400 bg-gray-200 dark:bg-gray-600 \
dark:text-red-500"
  "Reactbook.css classes for the HTML VERBATIM attribute."
  :type '(string))

(defcustom org-reactbook-class-link
  "text-blue-500 hover:underline dark:text-blue-400"
  "Reactbook.css classes for the HTML LINK attribute."
  :type '(string))

(defcustom org-reactbook-class-paragraph
  "my-2"
  "Reactbook.css classes for the HTML PARAGRAPH."
  :type '(string))

(defcustom org-reactbook-class-image-div
  "mx-8 my-12 max-w-full max-h-full"
  "Reactbook.css classes for the HTML image DIV."
  :type '(string))

(defcustom org-reactbook-class-image
  "mx-auto mb-2 max-w-full max-h-full rounded border \
border-gray-500"
  "Reactbook.css classes for the HTML IMAGE."
  :type '(string))

(defcustom org-reactbook-class-image-description
  "italic border-t border-gray-500"
  "Reactbook.css classes for the HTML image DESCRIPTION."
  :type '(string))

(defcustom org-reactbook-class-video-div
  "mx-8 my-12 max-w-full max-h-full"
  "Reactbook.css classes for the HTML video DIV."
  :type '(string))

(defcustom org-reactbook-class-video
  "mx-auto mb-2 rounded border border-gray-500"
  "Reactbook.css classes for the HTML VIDEO."
  :type '(string))

(defcustom org-reactbook-class-video-description
  "italic border-t border-gray-500"
  "Reactbook.css classes for the HTML video DESCRIPTION."
  :type '(string))

(defcustom org-reactbook-class-toc-title
  "text-md my-2 hover:bg-gray-300 dark:hover:bg-gray-600 \
block dark:border-gray-500"
  "Reactbook.css classes for the HTML Table of Contents title.
Use a single `\\' if you have line breaks in the string."
  :type '(string))

(defcustom org-reactbook-class-toc-items
  "text-md my-1 p-1 hover:bg-gray-100 dark:hover:bg-gray-600 \
dark:border-gray-500"
  "Reactbook.css classes for the HTML Table of Contents items.
Use a single `\\' if you have line breaks in the string."
  :type '(string))

(defcustom org-reactbook-class-current-toc
  "border-l-4 border-blue-400 dark:border-blue-500 bg-gray-100 \
  dark:bg-gray-600"
  "Reactbook.css classes for the CURRENT HTML Table of Contents item."
  :type '(string))

(defcustom org-reactbook-class-horizontal-rule
  "border-b-2 border-gray-300 dark:border-gray-700"
  "Reactbook.css classes for the HTML horizontal rule."
  :type '(string))


;; Lists

(defcustom org-reactbook-class-ordered-list
  "list-decimal my-2"
  "Reactbook.css classes for the HTML ORDERED list."
  :type '(string))

(defcustom org-reactbook-class-ordered-list-item
  "ml-10"
  "Reactbook.css classes for the HTML ordered list ITEM."
  :type '(string))

(defcustom org-reactbook-class-unordered-list
  "list-disc my-2"
  "Reactbook.css classes for the HTML UNORDERED list."
  :type '(string))

(defcustom org-reactbook-class-unordered-list-item
  "ml-10"
  "Reactbook.css classes for the HTML unordered list ITEM."
  :type '(string))

(defcustom org-reactbook-class-description-list
  "my-2 ml-8"
  "Reactbook.css classes for the HTML DESCRIPTION list."
  :type '(string))

(defcustom org-reactbook-class-description-list-title
  "font-bold border-b mr-72 dark:border-gray-700"
  "Reactbook.css classes for the HTML DESCRIPTION title."
  :type '(string))

(defcustom org-reactbook-class-description-list-item
  "ml-10"
  "Reactbook.css classes for the HTML description list ITEM."
  :type '(string))


;; Table

(defcustom org-reactbook-class-table-container
  "overflow-x-auto my-12"
  "Reactbook.css classes for the HTML table NAME."
  :type '(string))

(defcustom org-reactbook-class-table
  "table-auto m-auto my-2"
  "Reactbook.css classes for the HTML TABLE."
  :type '(string))

(defcustom org-reactbook-class-table-description
  "mx-48 text-center italic border-t border-gray-500"
  "Reactbook.css classes for the HTML table NAME."
  :type '(string))

(defcustom org-reactbook-class-table-header-row
  "text-gray-600 border-b-2 border-gray-400 bg-gray-100 \
  dark:text-gray-400 dark:bg-gray-600"
  "Reactbook.css classes for the HTML table HEADER-ROW."
  :type '(string))

(defcustom org-reactbook-class-table-header-cell
  "px-4 py-2 font-bold text-center"
  "Reactbook.css classes for the HTML table HEADER-CELL."
  :type '(string))

(defcustom org-reactbook-class-table-body-row
  "hover:bg-blue-100 dark:hover:bg-gray-600"
  "Reactbook.css classes for the HTML table BODY-ROW."
  :type '(string))

(defcustom org-reactbook-class-table-last-body-row
  "border-b-2 border-gray-400 hover:bg-blue-100 dark:hover:bg-gray-600"
  "Reactbook.css classes for the HTML table BODY-ROW."
  :type '(string))

(defcustom org-reactbook-class-table-body-cell
  "px-4 py-2"
  "Reactbook.css classes for the HTML table BODY-CELL."
  :type '(string))

(defcustom org-reactbook-class-table-empty-body-cell
  "px-2 py-2"
  "Reactbook.css classes for the HTML table EMPTY BODY-CELL."
  :type '(string))

;; Footnotes
(defcustom org-reactbook-class-footnotes-section
  "<div id=\"footnotes\">
  <h2 class=\"mt-20 mb-6 text-2xl text-gray-700 dark:text-gray-400 border-b \
  hover:text-green-500 dark:hover:text-blue-500 border-gray-500\">%s</h2> \
  <div id=\"text-footnotes\">
  %s
  </div>
  </div>"
  "Reactbook.css classes for the HTML footnotes section."
  :type '(string))

(defcustom org-reactbook-class-footnotes-format
  "<sup>%s</sup>"
  "Reactbook.css classes for the HTML footnotes format."
  :type '(string))

(defcustom org-reactbook-class-footnotes-separator
  "<sup>, </sup>"
  "Reactbook.css classes for the HTML footnotes separator."
  :type '(string))

;; Blocks

(defcustom org-reactbook-class-example-container
  "my-12 rounded border border-gray-800 shadow-xl"
  "Reactbook.css classes for the HTML EXAMPLE-BLOCK CONTAINER."
  :type '(string))

(defcustom org-reactbook-class-example
  "rounded rainbow-braces"
  "Reactbook.css classes for the HTML EXAMPLE-BLOCK."
  :type '(string))

(defcustom org-reactbook-class-src-container
  "my-12 shadow-xl"
  "Reactbook.css classes for the HTML SRC-BLOCK CONTAINER."
  :type '(string))

(defcustom org-reactbook-class-src-window
  "bg-gray-300"
  "Reactbook.css classes for the HTML SRC-BLOCK WINDOW.
This makes it look like the code is in an editor on mac OS."
  :type '(string))

(defcustom org-reactbook-class-pre
  "rainbow-braces match-braces rounded-b"
  "Reactbook.css classes for the HTML SRC-BLOCK."
  :type '(string))

(defcustom org-reactbook-class-blockquote-container
  "my-12"
  "Reactbook.css classes for the HTML BLOCKQUOTE container."
  :type '(string))

(defcustom org-reactbook-class-blockquote
  "mx-8 my-2 px-4 border-l-8 rounded border border-gray-500 \
  bg-gray-300 dark:bg-gray-600 dark:text-gray-300"
  "Reactbook.css classes for the HTML BLOCKQUOTE block."
  :type '(string))

(defcustom org-reactbook-class-blockquote-author
  "mx-8 italic border-t border-gray-500"
  "Reactbook.css classes for the HTML blockquote AUTHOR."
  :type '(string))


;; Special Blocks

(defcustom org-reactbook-class-mermaid-container
  "mx-8 my-12"
  "Reactbook.css classes for the HTML MERMAID container."
  :type '(string))

(defcustom org-reactbook-class-mermaid-block
  "mx-auto my-2 max-w-full max-h-full bg-white rounded border \
  border-gray-500"
  "Reactbook.css classes for the HTML MERMAID block."
  :type '(string))

(defcustom org-reactbook-class-mermaid-block-title
  "italic border-t border-gray-500"
  "Reactbook.css classes for the HTML MERMAID block."
  :type '(string))

(defcustom org-reactbook-class-details-block
  "my-4 py-2 px-8 rounded border-l-8 border border-gray-300 \
  dark:border-gray-600"
  "Reactbook.css classes for the HTML DETAILS block."
  :type '(string))

(defcustom org-reactbook-class-details-title
  "text-gray-500 font-bold"
  "Reactbook.css classes for the HTML details block TITLE."
  :type '(string))

(defcustom org-reactbook-class-anki-block
  "mb-2 px-10 text-xs"
  "Reactbook.css classes for the HTML DETAILS block."
  :type '(string))

(defcustom org-reactbook-class-anki-title
  "text-gray-300 dark:text-gray-600"
  "Reactbook.css classes for the HTML details block TITLE."
  :type '(string))

(defcustom org-reactbook-class-tip-block
  "my-12 p-8 rounded border-l-8 border border-blue-500 \
  shadow-xl"
  "Reactbook.css classes for the HTML TIP block."
  :type '(string))

(defcustom org-reactbook-class-tip-title
  "text-blue-500 font-bold"
  "Reactbook.css classes for the HTML tip block TITLE."
  :type '(string))

(defcustom org-reactbook-class-warning-block
  "my-12 p-8 rounded border-l-8 border border-yellow-500 \
  shadow-xl"
  "Reactbook.css classes for the HTML WARNING block."
  :type '(string))

(defcustom org-reactbook-class-warning-title
  "text-yellow-500 font-bold"
  "Reactbook.css classes for the HTML warning block TITLE."
  :type '(string))

(defcustom org-reactbook-class-danger-block
  "my-12 p-8 rounded border-l-8 border border-red-500 \
  shadow-xl"
  "Reactbook.css classes for the HTML DANGER block."
  :type '(string))

(defcustom org-reactbook-class-danger-title
  "text-red-500 font-bold"
  "Reactbook.css classes for the HTML danger block TITLE."
  :type '(string))


;; Page Divs

(defcustom org-reactbook-class-body
  "flex flex-col h-screen text-gray-700 dark:bg-darkgray \
  dark:text-gray-400"
  "Reactbook.css classes for the HTML BODY."
  :type '(string))

(defcustom org-reactbook-class-header
  "w-full border-b border-gray-500
  shadow-md items-center h-16"
  "Reactbook.css classes for the HTML HEADER."
  :type '(string))

(defcustom org-reactbook-class-sidebar
  "mx-12 md:mx-16 shadow-2xl rounded mt-12 p-12 md:p-16 lg:m-0 \
  lg:border-r lg:border-gray-500 lg:fixed lg:pt-2 lg:w-80 lg:px-2 \
  lg:overflow-y-auto lg:inset-y-0 lg:mt-16"
  "Reactbook.css classes for the HTML SIDEBAR."
  :type '(string))

(defcustom org-reactbook-class-content
  "flex flex-col lg:flex-row overflow-y-auto"
  "Reactbook.css classes for the HTML CONTENT."
  :type '(string))

(defcustom org-reactbook-class-content-container
  "flex-grow px-8 py-12 sm:px-12 md:px-16 lg:ml-80 lg:px-20 \
  lg:overflow-x-auto xl:px-32 2xl:px-48"
  "Reactbook.css classes for the HTML contents CONTAINER."
  :type '(string))

(defcustom org-reactbook-class-inner-container
  "relative -top-16 p-16 pb-32 mb-12 shadow-2xl rounded \
  xl:p-24"
  "Reactbook.css classes for the HTML inner container."
  :type '(string))

(defcustom org-reactbook-class-footer
  "hidden fixed bottom-0 w-full border-t border-solid \
  border-gray-500 h-8 text-center bg-white"
  "Reactbook.css classes for the HTML FOOTER."
  :type '(string))

(defcustom org-reactbook-class-toggle-button
  "float-right rounded px-4 py-1 border bg-black bg-gray-100 \
  dark:bg-gray-300 dark:text-gray-700"
  "Reactbook.css classes for the HTML go to Toggle dark-mode button.
There are already some prefixed classes:
  - p-2
  - block
  - mt-2
  - ml-0"
  :type '(string))

(defcustom org-reactbook-class-top-button
  "absolute right-0 bottom-0 mb-2 mr-8 z-50 bg-gray-500 \
  bg-opacity-80 hover:bg-gray-700 text-white font-bold rounded \
  h-10 w-16 flex items-center justify-center"
  "Reactbook.css classes for the HTML go to TOP button.
There are already some prefixed classes:
  - p-2
  - block
  - mt-2
  - ml-0"
  :type '(string))

(defcustom org-reactbook-class-search-bar
  "float-right mx-4 w-1/6 rounded px-4 py-1 border-solid \
  border border-gray-700 dark:border-gray-500 text-gray-400 \
  dark:text-gray-900 focus:border-green-500 \
  dark:focus:border-blue-500 focus:text-gray-500 \
  dark:focus:text-gray-500 dark:bg-midgray"
  "Reactbook.css classes for the HTML SEARCH BAR."
  :type '(string))

(defcustom org-reactbook-class-search-bar-results-list
  "z-50 absolute w-5/6 sm:w-4/6 md:w-3/6 lg:w-2/6 xl:w-1/6
  right-0 mt-12 mr-20 bg-white dark:bg-midgray p-4 shadow-lg border
  border-solid border-gray-500 rounded"
  "Reactbook.css classes for the HTML RESULTS LIST."
  :type '(string))

(defcustom org-reactbook-class-search-bar-results-item
  "p-2 block rounded hover:bg-gray-300 dark:hover:bg-darkgray"
  "Reactbook.css classes for the HTML RESULTS ITEM.
Do not break the line with while inserting a `newline'.  Use `\' at
  the end."
  :type '(string))


;; Other elements

(defcustom org-reactbook-class-checkbox
  "form-tick appearance-none h-6 w-6 mr-2 border border-gray-300 \
  rounded checked:bg-blue-600 checked:border-transparent \
  focus:outline-none align-text-bottom"
  "Reactbook.css classes for Checkbox."
  :type '(string))

(defcustom org-reactbook-class-file-name
  "relative top-10 flex rounded-t py-4 px-16 text-sm xl:px-24 z-10"
  "Reactbook.css classes for File Name."
  :type '(string))


;;; Templates

(defcustom org-reactbook-file-name-use-link t
  "Whether to use a link or a paragraph for the file-name field."
  :type '(boolean))


(defcustom org-reactbook-file-name-link
  "<a class=\"%s\" href=\"org-protocol://open-file?file=%s\">%s</a>"
  "The link to open the file in Emacs - preferably with org-protocol."
  :type '(string))


(defcustom org-reactbook-head-files
  "<!-- Reactbook CSS -->
  <link href=\"./css/reactbook.css\" rel=\"stylesheet\"/>

  <!-- Prism Css -->
  <link href=\"./css/prism.css\" rel=\"stylesheet\" />

  <!-- Mathjax -->
  <script>
  MathJax = {
  tex: {
  processEscapes: true
  }
  }
  </script>

  <script type=\"text/javascript\" async
  src=\"./mathjax/tex-mml-chtml.js\">
  </script>

  <!-- Your CSS file should come here -->
  <link href=\"./css/style.css\" rel=\"stylesheet\" />

  <!-- Toc tree file -->
  <script src=\"./js/toc_tree.js\"></script>
  "
  "Links to be imported on the head of the HTML file."
  :type '(string))

(defcustom org-reactbook-header
  "<nav class=\"flex items-center justify-between flex-wrap p-4\">
  <div class=\"flex items-center flex-no-shrink mr-6\">
  <span class=\"font-semibold text-xl tracking-tight\">
  <a href=\"./index.html\">Notes</a>
  </span>
  </div>
  <div class=\"block flex-grow lg:flex lg:items-center lg:w-auto\">
  <div class=\"text-sm lg:flex-grow\">
  <a href=\"#top\" class=\"%s\">
  Top
  </a>
  <input id=\"search-bar\" onkeyup=\"search()\"
  onfocusin='showResults()'
  class=\"%s\" placeholder=\"Search...\"/>
  <ul id=\"search-bar-results\"
  class=\"%s\" style=\"display: none;\"></ul>
  <button type=\"button\" class=\"%s\" onclick=\"toggleLight()\">Toggle dark-mode</button>
  </div>
  </div>
  </nav>"
  "Contents of header in HTML."
  :type '(string))

(defcustom org-reactbook-sidebar
  "<div id=\"toc\"></div>"
  "Contents of sidebar in HTML."
  :type '(string))

(defcustom org-reactbook-footer
  "<p>Exported with org-reactbook</p>"
  "Contents of footer in HTML."
  :type '(string))

(defcustom org-reactbook-bottom-files
  "<script src=\"./js/prism.js\"></script>
  <script src=\"./js/mermaid.min.js\"></script>
  <script>mermaid.initialize({startOnLoad:true});</script>"
  "Javascript files to be imported at the bottom of the HTML file."
  :type '(string))

(defcustom org-reactbook-headlines
  "h1,h2,h3,h4"
  "The level of the headlines to be included in the toc."
  :type '(string))

(defcustom org-reactbook-app-template "
function App() {
  const [tocOpen, setTocOpen] = useState(false);

  const handleDrawerOpen = () => {
    setTocOpen(true);
  };

  const handleDrawerClose = () => {
    setTocOpen(false);
  };

  return (
    <div className={`bg-gray-50 ${tocOpen && \"md:pl-48\"}`}>
      <div className=\"flex items-center p-4\">
        <button onClick={handleDrawerOpen} className={tocOpen && \"md:hidden\"}>
          <FcMenu className=\"text-xl\" />
        </button>

        <div className=\"pl-12 text-2xl font-barlow\">
          %s
        </div>
      </div>

      <Drawer variant=\"persistent\" anchor=\"left\" open={tocOpen}>
        <div className=\"px-4 w-56\">
          <div className=\"mb-2 py-6 flex items-center\">
            <button onClick={handleDrawerClose}>
              <BsChevronLeft className=\"text-lg\" />
            </button>
          </div>

          <div className=\"text-lg font-barlow font-bold mb-2\">Table of Contents</div>

          {chapters.map((chapter) => (
            <Link to={chapter[1]} class=\"text-gray-600 text-sm font-barlow block hover:text-black\">
              {chapter[0]}
            </Link>
          ))}
        </div>
      </Drawer>
      <Outlet />
    </div>
  );
}
"
  "React code for the App container"
  :type '(string))

(defun org-reactbook-app (title)
  (format org-reactbook-app-template title))

(defcustom org-reactbook-html-template
  "
import { useState } from 'react'
import Drawer from '@mui/material/Drawer'
import { BrowserRouter as Router, Link, Outlet, Routes, Route } from 'react-router-dom'
import { FcMenu } from 'react-icons/fc'
import { BsChevronLeft } from 'react-icons/bs'

import { ChapterTitle, Paragraph, Quote, SectionTitle } from './Components'

%s

const chapters = [%s]

%s

function Main() {
  return (
    <Router>
      <Routes>
        <Route path=\"/\" element={<App />}>
          %s
        </Route>
      </Routes>
    </Router>
  )
}

export default Main
"
  "Default HTML template.
The default template needs the format place for the following
values, in this order:

- page title
- head files
- body classes
- content classes
- content-container classes
- inner-container classes
- contents"
  :type '(string))

(defun org-reactbook-chapter-title2component-name (title)
  (s-replace-all '(
                   (" " . "_")
                   ("?" . "")
                   (":" . "")
                   )
                 title))

(defun org-reactbook-chapter-title2url-path (title)
  (s-downcase (org-reactbook-chapter-title2component-name title)))

(defun org-reactbook-toc-pairs ()
  (mapconcat #'(lambda (chapter)
                 (format "[\"%s\", \"%s\"]"
                         chapter
                         (org-reactbook-chapter-title2url-path chapter)))
             (reverse org-reactbook-chapters)
             ", "))

(defun org-reactbook-routes ()
  (mapconcat #'(lambda (chapter)
                 (format "<Route path=\"%s\" element={<%s />} />"
                         (org-reactbook-chapter-title2url-path chapter)
                         (org-reactbook-chapter-title2component-name chapter)))
             org-reactbook-chapters "\n"))

(defun org-reactbook-template (contents info)
  "Format the HTML Template and add the CONTENTS of the export."
  (when (plist-get info :with-title)
    (let ((title (car (or
                       (plist-get info :title)
                       '("")))))
      (format org-reactbook-html-template
              contents
              (org-reactbook-toc-pairs)
              (org-reactbook-app title)
              (org-reactbook-routes)))))

;;; Transcode Elements

(defun org-reactbook--file-name-link-or-paragraph (info)
  "Write the file name as a link or paragraph.
Depends on the variable `org-reactbook-file-name-use-link'."
  (if org-reactbook-file-name-use-link
      (format org-reactbook-file-name-link
              org-reactbook-class-link
              (plist-get info :input-file)
              (plist-get info :input-buffer))
    (format "<p>%s</p>"
            (plist-get info :input-buffer))))


(defun org-reactbook-inner-template (contents info)
  "Define the template for the CONTENTS inside Headings.
By not doing anything to the contents, it exports the elements at the root level."
  (concat contents (org-html-footnote-section info)))

(defun org-reactbook-bold (bold contents info)
  "Transcode BOLD from Org to HTML."
  (format "<strong class=\"%s\">%s</strong>" org-reactbook-class-bold contents))


(defun org-reactbook-italic (italic contents info)
  "Transcode ITALIC from Org to HTML."
  (format "<em class=\"%s\">%s</em>" org-reactbook-class-italic contents))


(defun org-reactbook-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML."
  (format "<del class=\"%s\">%s</del>" org-reactbook-class-italic contents))


(defun org-reactbook-underlined (underlined contents info)
  "Transcode UNDERLINED from Org to HTML."
  (format "<span class=\"%s\">%s</span>" org-reactbook-class-underlined contents))


(defun org-reactbook-code (code contents info)
  "Transcode CODE from Org to HTML."
  (let* ((code-text (org-element-property :value code))
        (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                             (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                        r2)))
    (format "<code class=\"%s\">%s</code>" org-reactbook-class-code escaped-text)))


(defun org-reactbook-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to HTML."
  (let* ((code-text (org-element-property :value verbatim))
         (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                              (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                         r2)))
    (format "<code class=\"%s\">%s</code>" org-reactbook-class-verbatim escaped-text)))


(defun org-reactbook-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to HTML."
  (let* ((code-text (org-element-property :value verbatim))
         (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                              (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                         r2)))
    (format "<code class=\"%s\">%s</code>" org-reactbook-class-verbatim escaped-text)))


(defun org-reactbook-horizontal-rule (_horizontal-rule _contents info)
  "Transcode HORIZONTAL-RULE from Org to HTML."
  (format "<hr class=\"%s\">"
          org-reactbook-class-horizontal-rule))


(defun org-reactbook-checkbox (checkbox)
  "Format a checkbox item into the corresponding HTML tag."
  (let ((checkbox-tag "<input type=\"checkbox\" class=\"%s\" disabled %s>"))
    (pcase checkbox
      (`on (format checkbox-tag
                   org-reactbook-class-checkbox "checked"))
      (`off (format checkbox-tag
                    org-reactbook-class-checkbox "unchecked"))
      (_ (format checkbox-tag
                 org-reactbook-class-checkbox "unchecked")))))


(defun org-reactbook-paragraph (paragraph contents info)
  "Transcode PARAGRAPH from Org to HTML."
  (let* ((parent (org-element-property :parent paragraph))
         (parent-type (org-element-property :type parent))
         (ancestor (org-element-property :parent parent))
         (ancestor-type (org-element-property :type ancestor))
         (checkbox (org-element-property :checkbox parent))
         (is-checkbox-p (unless (equalp checkbox nil) t))
         (is-image-p (string-match-p "<img " contents))
                 (is-video-p (string-match-p "<video " contents)))
    (cond
     ;; For Mermaid.js return raw text
     ((equalp parent-type "mermaid") contents)
     ;; For Descriptive types, surround with `dd' tag
     ((equalp ancestor-type 'descriptive)
      (format "<dd class=\"%s\">%s</dd>"
              org-reactbook-class-description-list-item
              contents))
     ;; Put image inside a div with description
     (is-image-p (format "<div class=\"%s\">%s</div>"
                         org-reactbook-class-image-div
                         contents))
         ;; Put video inside a div with description
     (is-video-p (format "<div class=\"%s\">%s</div>"
                         org-reactbook-class-video-div
                         contents))
     ;; If is a checkbox
     (is-checkbox-p
      (format "<p class=\"%s\">%s %s</p>"
              org-reactbook-class-paragraph
              (org-reactbook-checkbox checkbox)
              contents))
     ;; Normal paragraph
    (t (format "<Paragraph>\n%s\n</Paragraph>\n\n" contents)))))


(defun org-reactbook-target (target _contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((ref (org-element-property :value target)))
    (format "<span id=\"%s\"></span>" ref)))


(defun org-reactbook-link (link contents info)
  "Transcode LINK from Org to HTML."
  (let* ((path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (type (org-element-property :type link))
         (parent (org-element-property :parent link))
         (description (org-element-property :name parent))
         (timeline (org-reactbook--get-attribute :attr_timeline parent))
         (file-extension (file-name-extension path))
         (link-tag "<a class=\"%s\" href=\"%s\">%s</a>")
         (video-tag "<video class=\"%s\" controls><source src=\"%s\" type=\"video/%s\"/></video>%s")
         (link-description "<p class=\"%s\">%s</p>")
         (image-tag "<img class=\"%s\" src=\"%s\"/>%s"))
    (cond
     ;; Is the link a video
     ((or (equalp file-extension "mp4")
          (equalp file-extension "avi")
          (equalp file-extension "mkv")
          (equalp file-extension "webm")
          (equalp file-extension "mpeg4")
          (equalp file-extension "3gp"))
      (format video-tag
              org-reactbook-class-video
              (if timeline
                  (format "%s#t=%s" path timeline)
                path)
              file-extension
              (if description
                  (format link-description
                          org-reactbook-class-video-description
                          description)
                "")))
     ;; Is the link an image
     ((or (equalp file-extension "png")
          (equalp file-extension "svg")
          (equalp file-extension "tiff")
          (equalp file-extension "tif")
          (equalp file-extension "jpg")
          (equalp file-extension "jpeg")
          (equalp file-extension "webp")
          (equalp file-extension "gif")
          (equalp file-extension "bmp"))
      (format image-tag
              org-reactbook-class-image
              path
              (if description
                  (format link-description
                          org-reactbook-class-image-description
                          description)
                "")))
     ((equalp type "org-protocol")
      (format link-tag
              org-reactbook-class-link
              raw-link
              contents))
     ;; Is the link an org file
     ((equalp file-extension "org")
      (format link-tag
              org-reactbook-class-link
              (concat type ":" (replace-regexp-in-string "\\.org" ".html" path))
              contents))
     ;; Internal link
     ((equalp type "fuzzy")
      (format link-tag
              org-reactbook-class-link
              (concat "#" raw-link)
              (if contents
                  contents
                (concat "#" raw-link))))
     ;; Any other link
     (t (format link-tag
                org-reactbook-class-link
                (concat type ":" path)
                (if contents
                    contents
                  (concat type ":" path)))))))


(defun org-reactbook-blockquote (blockquote contents info)
  "Transcode BLOCKQUOTE from Org to HTML."
  (format "<Quote>%s</Quote>" contents))


(defvar org-reactbook--src-block-open
  "<div class=\"%s\">%s<pre class=\"%s\" %s>"
  "Opening tag of code block for Prism.js.
It has two format places:
- Reactbook.css classes
- other attribute.")


(defvar org-reactbook--src-block-close
  "<code class=\"language-%s\">%s</code></pre></div>"
  "Closing tag of code block for Prism.js.
It has two format places:
- language
- code text")


(defvar org-reactbook--src-block-file-name
  "<div class=\"h-8 rounded-t flex %s\">
  <div class=\"bg-red-500 my-2 ml-2 mr-1 h-4 w-5 rounded-full\"></div>
  <div class=\"bg-yellow-500 my-2 ml-1 mr-1 h-4 w-5 rounded-full\"></div>
  <div class=\"bg-green-500 my-2 ml-1 mr-1 h-4 w-5 rounded-full\"></div>
  <p class=\"ml-2 mr-16 my-1 w-full text-center dark:text-gray-700\">%s</p>
</div>"
  "Make it look like the code is in a mac OS code editor.")


(defun org-reactbook--get-attribute (attribute block)
  "Get the ATTRIBUTE from an org BLOCK element."
  (car (org-element-property attribute block)))


(defun org-reactbook-command-line-block (src-block contents info)
  "Transcode SRC-BLOCK with command line language to a custom Prism.js code block."
  (let* ((code-text (org-element-property :value src-block))
         (language (org-element-property :language src-block))
         (filepath (org-reactbook--get-attribute :attr_filepath src-block))
         (file-name (org-reactbook--get-attribute :attr_filename src-block))
         (username (org-reactbook--get-attribute :attr_username src-block))
         (hostname (org-reactbook--get-attribute :attr_hostname src-block))
         (highlight-lines (org-reactbook--get-attribute :attr_highlight src-block)))
    (concat
     (format org-reactbook--src-block-open
             org-reactbook-class-src-container
             (format org-reactbook--src-block-file-name
                     org-reactbook-class-src-window
                     (if file-name
                         file-name
                       "Console"))
             (concat "command-line " org-reactbook-class-pre)
             (concat
              "data-filter-output=\"(out)\" "
              ;; Add user and host names
              (format " data-user=\"%s\" data-host=\"%s\""
                      (if username username "user")
                      (if hostname hostname "localhost"))
              (if highlight-lines
                  (format " data-line=\"%s\"" highlight-lines)
                "")
              ;; Add file src
              (if filepath
                  (format " data-src=\"%s\" data-download-link" filepath)
                "")
              ))
     (format org-reactbook--src-block-close
             (if (equalp language "ps") "powershell" language)
             code-text))))


(defun org-reactbook-src-block (src-block contents info)
  "Transcode SRC-BLOCK from Org to HTML."
  (let* ((code-text (org-element-property :value src-block))
         (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                              (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                         r2))
         (language (org-element-property :language src-block))
         (filepath (org-reactbook--get-attribute :attr_filepath src-block))
         (file-name (org-reactbook--get-attribute :attr_filename src-block))
         (highlight-lines (org-reactbook--get-attribute :attr_highlight src-block))
         (fetch-code (org-reactbook--get-attribute :attr_fetch src-block)))
    (concat
     (format org-reactbook--src-block-open
             org-reactbook-class-src-container
             (format org-reactbook--src-block-file-name
                     org-reactbook-class-src-window
                     (if file-name
                         file-name
                       ""))

             (concat "line-numbers " org-reactbook-class-pre)
             (concat " "
                     (if highlight-lines
                         (format " data-line=\"%s\"" highlight-lines)
                       "")
                     (if fetch-code
                         (format " data-jsonp=\"%s\" language-%s"
                                 fetch-code language)
                       "")
                     ;; Add file src
                     (if filepath
                         (format " data-src=\"%s\" data-download-link" filepath)
                       "")))
     (format org-reactbook--src-block-close
             language
             escaped-text))))


(defun org-reactbook-src-block-select (src-block contents info)
  "Transcode SRC-BLOCK from Org to HTML."
  (let* ((language (org-element-property :language src-block)))
    (cond
     ;; If it is a command line language,
     ((or (equalp language "sh")
          (equalp language "shell")
          (equalp language "bash")
          (equalp language "ps")
          (equalp language "powershell"))
      (org-reactbook-command-line-block src-block contents info))
     ;; If it is a programming language
     (t (org-reactbook-src-block src-block contents info)))))

(defun org-reactbook-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK from Org to HTML."
  (let ((code (org-html-format-code example-block info))
        (code-text (org-element-property :value example-block))
        (language (org-element-property :language example-block))
        (highlight-lines (org-element-property :highlight example-block)))
    (format "<div class=\"%s\">
<pre class=\"%s\">
<code class=\"language-%s\">%s</code>
</pre>
</div>"
            org-reactbook-class-example-container
            org-reactbook-class-example
            (if language language "none")
            code-text)))


(defun org-reactbook-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK from Org to HTML.
There are 4 types of blocks:
- Details
- Tip
- Warning
- Danger"
  ;; HACK: In order not to add the p tags in paragraph if the special-block's
  ;; type is `mermaid', change the paragraph block directly
  (let* ((type (org-element-property :type special-block))
         (name (org-element-property :name special-block)))
    (cond ((equalp type "mermaid")
           (format "<div class=\"%s\"><div class=\"mermaid %s\">%s</div><p class=\"%s\">%s</p></div>"
                   org-reactbook-class-mermaid-container
                   org-reactbook-class-mermaid-block
                   contents
                   org-reactbook-class-mermaid-block-title
                   (if name name "")))
          ;; TODO: process title as summary
          ((equalp type "ANKI")
           (format "<details class=\"%s\"><summary class=\"%s\">%s</summary>%s</details>"
                   org-reactbook-class-anki-block
                   org-reactbook-class-anki-title
                   (if name name "Anki Card")
                   contents))
          ((equalp type "details")
           (format "<details class=\"%s\"><summary class=\"%s\">%s</summary>%s</details>"
                   org-reactbook-class-details-block
                   org-reactbook-class-details-title
                   (if name name "Details")
                   contents))
          ((equalp type "tip")
           (format "<div class=\"tip %s\"><p class=\"%s\">%s</p>%s</div>"
                   org-reactbook-class-tip-block
                   org-reactbook-class-tip-title
                   (if name name "")
                   contents))
          ((equalp type "warning")
           (format "<div class=\"warning %s\"><p class=\"%s\">%s</p>%s</div>"
                   org-reactbook-class-warning-block
                   org-reactbook-class-warning-title
                   (if name name "")
                   contents))
          ((equalp type "danger")
           (format "<div class=\"danger %s\"><p class=\"%s\">%s</p>%s</div>"
                   org-reactbook-class-danger-block
                   org-reactbook-class-danger-title
                   (if name name "")
                   contents))
          (t (org-html-special-block special-block contents info)))))

(defun org-reactbook-chapter (title contents)
  (let* ((component-name (org-reactbook-chapter-title2component-name title))
         (template "
function %s() {
  return (
    <div class=\"md:pl-56 bg-gray-50\">
      <div class=\"p-8 border-l-2 border-gray-300 font-serif min-h-screen max-w-2xl\">
        <ChapterTitle>%s</ChapterTitle>

        %s
      </div>
    </div>
  )
}\n\n"))
    (format template component-name title contents)))

(defun org-reactbook-section-title (level text contents)
  (format "<SectionTitle level={%s}>%s</SectionTitle>\n\n%s" level text contents))

(defun print-level (level)
  (print (if (arrayp level) (format "%s" (substring level 0 25)) level)))

(defun org-reactbook-format-header (level text contents)
  "Return the corresponding HTML heading level of the Org LEVEL."
  (let ((header-contents (if contents contents ""))
        (header "<h%s class=\"%s\">%s</h%s>%s" ))
    (cond
     ((eq level 1) (progn
                     (push text org-reactbook-chapters)
                     (org-reactbook-chapter text header-contents)))
     ((eq level 2) (org-reactbook-section-title level text header-contents))
     ((eq level 3) (org-reactbook-section-title level text header-contents))
     ((eq level 4) (org-reactbook-section-title level text header-contents))
     ((eq level 5) (org-reactbook-section-title level text header-contents))
     (t (format header level org-reactbook-class-h7 text level header-contents)))))


(defun org-reactbook-headline (headline contents info)
  "Transcode HEADLINE from Org to HTML."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((text (org-element-property :raw-value headline))
           (level (org-export-get-relative-level headline info)))
      (print-level level)
      (org-reactbook-format-header level text contents))))


(defun org-reactbook-table-cell (table-cell contents info)
  "Transcode TABLE-CELL from Org to HTML.
If TABLE-CELL is part of the table header, return the HTML table
cell with `th'.  Return `td' otherwise."
  (let* ((header-p (org-export-table-row-in-header-p
                   (org-export-get-parent table-cell) info))
         (text-alignment (org-export-table-cell-alignment
                          table-cell
                          info)))
    (if header-p
        (format "<th class=\"%s\">%s</th>" org-reactbook-class-table-header-cell contents)
      (if contents
          (format "<td class=\"%s %s\">%s</td>"
                  org-reactbook-class-table-body-cell
                  (concat "text-"
                          (cond
                           ((equal text-alignment 'left) "left")
                           ((equal text-alignment 'center) "center")
                           ((equal text-alignment 'right) "right")))
                  contents)
        (format "<td class=\"%s\"></td>"
                org-reactbook-class-table-empty-body-cell)))))

(defun org-reactbook-table-row (table-row contents info)
  "Transcode TABLE-ROW from Org to HTML.
First, check the group of the TABLE-ROW, in this case it will be
either `thead' or `tbody'.  Second, check if it is the first row
of the group.  If it is, add the corresponding opening HTML tag.
In the case of being the last row of the group, add the closing
tag.  If it is neither, return the row without group tags."
  (let* ((header '("<thead>" . "</thead>"))
         (body '("<tbody>" . "</tbody>"))
         (row '("<tr class=\"%s\">" . "</tr>"))
         (row-group (org-export-table-row-group table-row info))
         (is-first-row-p (org-export-table-row-starts-rowgroup-p table-row info))
         (is-last-row-p (org-export-table-row-ends-rowgroup-p table-row info)))
    ;; As there are multiple types of rows, e.g., separator rows,
    ;; check if the row is a normal row
    (when (eq (org-element-property :type table-row) 'standard)
      ;; If row is in the table header
      (if (eq row-group 1)
          (cond
           ;; Is the first row of the header
           ((not (eq is-first-row-p nil))
            (concat (car header)
                    (format (car row) org-reactbook-class-table-header-row)
                    contents
                    (cdr row)))
           ;; Is the last row of the header
           ((not (eq is-last-row-p nil))
            (concat (format (car row) org-reactbook-class-table-header-row)
                    contents
                    (cdr row)
                    (cdr header)))
           ;; Is in the middle of header
           (t (concat (format (car row) org-reactbook-class-table-header-row)
                      contents
                      (cdr row))))
        ;; If row is in the table body
        (cond
         ;; Is the first row of the body
         ((not (eq is-first-row-p nil))
          (concat (car body)
                  (format (car row) org-reactbook-class-table-body-row)
                  contents
                  (cdr row)))
         ;; Is the last row of the body
         ((not (eq is-last-row-p nil))
          (concat (format (car row) org-reactbook-class-table-last-body-row)
                  contents
                  (cdr row)
                  (cdr body)))
         ;; Is in the middle of the body
         (t (concat (format (car row) org-reactbook-class-table-body-row)
                    contents
                    (cdr row))))))))

(defun org-reactbook-table (table contents info)
  "Transcode TABLE from Org to HTML."
  (let ((name (org-element-property :name table)))
    (format "<div class=\"%s\"><table class=\"%s\">%s</table>%s</div>"
            org-reactbook-class-table-container
            org-reactbook-class-table
            contents
            (if name
                (format "<p class=\"%s\">%s</p>"
                        org-reactbook-class-table-description
                        name)
              ""))))

(defun org-reactbook-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST from Org to HTML.
There are three types of lists:
- Ordered Lists;
- Unordered Lists and
- Description Lists."
  (let* ((type (pcase (org-element-property :type plain-list)
                 (`ordered "ol")
                 (`unordered "ul")
                 (`descriptive "dl")
                 (other (error "Unknown HTML list type: %s" other)))))
    (format "<%s class=\"%s\">%s</%s>"
            type
            (cond ((equalp type "ol") org-reactbook-class-ordered-list)
                  ((equalp type "ul") org-reactbook-class-unordered-list)
                  ((equalp type "dl") org-reactbook-class-description-list))
            contents
            type)))

(defun org-reactbook-format-list-item (contents type checkbox info
                                           &optional term-counter-id headline)
  "Format a list item into the corresponding HTML tag."
  (cond
   ((equalp type `ordered)
    (format "<li class=\"%s\">%s</li>"
            org-reactbook-class-ordered-list-item contents))
   ((equalp type `unordered)
    (format "<li class=\"%s\">%s</li>"
            org-reactbook-class-unordered-list-item contents))
   ((equalp type `descriptive)
    (format "<dt class=\"%s\">%s</dt>%s"
            org-reactbook-class-description-list-title
            term-counter-id
            contents))))

(defun org-reactbook-item (item contents info)
  "Transcode an ITEM element from Org to HTML."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (counter (org-element-property :counter item))
         (checkbox (org-element-property :checkbox item))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info)))))
    (org-reactbook-format-list-item
     contents type checkbox info (or tag counter))))

(defun org-reactbook-section (section contents info)
  "Return the contents of SECTION as is."
  contents)


;;; Search bar toc tree

(defun org-reactbook--json-toc-item (file title heading index)
  "Transform HEADING and TITLE to a json object."
  (format
   "\t{\n\t\t\"name\": \"%s\", \n\t\t\"index\": \"%s\", \n\t\t\"parent\": \"%s\", \n\t\t\"file\": \"%s\"\n\t},"
   (replace-regexp-in-string "\"" "'" heading)
   index
   title
   (replace-regexp-in-string "\\.org$" ".html" file)))


(defun org-reactbook--json-toc-all-items (headings title file)
  "Transform all the HEADINGS into an object with the heading and the TITLE."
  (string-join
   (-map
    (lambda (heading)
      (org-reactbook--json-toc-item
       file
       title
       (-first-item heading)
       (-last-item heading)))
    headings)
   "\n"))


(defun org-reactbook--json-toc (filename file-tree output-directory)
  "Read all the exported files and search for headings.
The JSON format will"

  ;; Create an empty list to append all the headings
  (setq headings '())

  (setq headings-max-level (+ 1 (s-count-matches "," org-reactbook-headlines)))

  ;; Create a temporary buffer to store the text of the file
  (with-temp-buffer
    ;; Paste all the text inside the file to the temp buffer
    (insert-file-contents filename)
    ;; Start the index count
    ;; this will be used to target the element position in html
    (setq count 0)
    ;; Get the title
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title:\s\\(.*\\)$" nil t)
        (progn
          (setq headings (append headings `((,(match-string 1) ,count))))
          (setq count (+ count 1))))
    ;; Search for the org headings
    (goto-char (point-min))
    (while (re-search-forward
            (format "^\\*\\{1,%s\\}\s.*$" headings-max-level)
            nil
            t)
      ;; Split the org headings and text
      (let ((cur-line (string-join (cdr (split-string (match-string 0) "\s")) " ")))
        (setq headings (append headings `((,cur-line ,count))))
        (setq count (+ count 1)))))

  (setq title (-first-item (-first-item headings)))
  (setq json-headings
        (org-reactbook--json-toc-all-items headings title filename))

  ;; Write the json toc tree to file
  (write-region json-headings nil file-tree 'append))


(defun org-reactbook--json-toc-all-files (notes-directory output-directory)
  "Read all the files in the NOTES-DIRECTORY and create a toc tree."

  (setq files (directory-files notes-directory nil ".org"))

  (setq file-tree (concat output-directory "/js/" "toc_tree.js"))

  ;; Clear the file
  (write-region "" nil file-tree)

  ;; Open the js object
  (write-region "const tocTree = [\n" nil file-tree 'append)

  ;; Append toc
  (-map
   (lambda (file)
     (let ((file-path (concat "./" file)))
       (org-reactbook--json-toc file-path file-tree output-directory))) files)

  ;; Close the js object
  (write-region "\n];" nil file-tree 'append))


;;;###autoload
(defun org-reactbook-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a REACTBOOK HTML buffer.
Export as `org-html-export-as-html' does, with slimhtml
org-export-backend.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Export is done in a buffer named \"*Org REACTBOOK export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'reactbook "*Org REACTBOOK Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-reactbook-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML file.
Export as `org-html-export-as-html' does, with slimhtml
org-export-backend.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Return output file's name."
  (interactive)
  (let* ((extension (concat "." "js"))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (setq-local org-reactbook-chapters '())
    (setq-local org-reactbook-book-title "")
    (org-export-to-file 'reactbook file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-reactbook-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-reactbook--json-toc-all-files (file-name-directory filename) pub-dir)
  (org-publish-org-to 'reactbook filename (concat "." "js") plist pub-dir))

;;;###autoload
(defun org-reactbook-publish-to-html-without-toc (plist filename pub-dir)
  "Publish an org file to HTML without toc to build faster.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'reactbook filename (concat "." "js") plist pub-dir))

;;;###autoload
(defun org-reactbook-build-toc ()
  "Build the toc for the current buffer.
Open one if the notes and then run this function."
  (interactive)
  (org-reactbook--json-toc-all-files (file-name-directory (buffer-file-name)) "./dist/"))

(provide 'ox-reactbook)
;;; ox-reactbook.el ends here
