;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'org-id)
(require 'org-roam-node)
(defun xfw/get-buffer-section (section-title)
  "Copy the content of the SECTION-TITLE  section."
  (interactive "sSection Title:")
  (save-excursion
    (let ((start nil)
          (end nil))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat "^\\*+ " section-title) nil t)
          (setq start (line-end-position))
          (when (re-search-forward "^\\*+ " nil t)
            (setq end (line-beginning-position))))
        (if (and start end)
	  (buffer-substring start end)
	  (if start
	       (buffer-substring start (point-max))
	       (message "%s not found!" section-title)
	       (progn nil)
               )
	  )
	))))

(defun xfw/append-content-to-file (content file-path)
  "Append the CONTENT to the FILE-PATH file."
  (interactive)
  (with-temp-buffer
    (insert content)
    (append-to-file (point-min) (point-max) file-path)))


(defun xfw/extract-tags-from-org ()
  "Extract all tags from the current org document."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((tagstring nil)
	  (tags nil))
	  (if (re-search-forward "^#\\+filetags.*\n" nil t)
	      (setq tagstring (substring-no-properties (match-string 0)))
	    (setq tagstring " "))
	  (setq tags(string-split tagstring ":"))
	  tags
	  )
  ))

(defun xfw/link-source ()
  "Get the id of the file, and format the id string, and tag the file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((id nil)
	  (externid nil))
    (setq id (org-id-get))
    (setq externid (concat "* [[id:" id "][" "Source Link]]"))
    )))

(defun xfw/copy-content (tag section-title dest-file)
  "Copy SECTION-TITLE part of content form the buffer with TAG to DEST-FILE."
  (interactive)
  (if (null (member tag (xfw/extract-tags-from-org)))
      (message "Current buffer doesn't contain the tag \"%s\"!" tag)
    ( let ((content nil))
      (setq content (xfw/get-buffer-section section-title))
      (if content
	  (let ((catcontent nil))
	    (setq catcontent (concat (xfw/link-source) content))
	    (xfw/append-content-to-file catcontent dest-file)
	    )
	(message "Content is nil")
	  ))
    )
  )

(defun xfw/refile ()
  "Refile the article reading."
  (interactive)
  (xfw/copy-content "articlereading" "概要" "d:/Gitlocal/Notes/roam/summary/summary2023070611-概要.org")
  (xfw/copy-content "articlereading" "亮点" "d:/Gitlocal/Notes/roam/summary/summary2023070611-文献亮点.org")
  (xfw/copy-content "articlereading" "结论" "d:/Gitlocal/Notes/roam/summary/summary2023070611-文献结论.org")
  (xfw/copy-content "articlereading" "想法" "d:/Gitlocal/Notes/roam/summary/summary2023070611-想法.org")
  (xfw/copy-content "articlereading" "不足" "d:/Gitlocal/Notes/roam/summary/summary2023070611-文献不足.org")
  )
(provide 'xfw-orgroam)
;;; xfw-orgroam.el ends here
