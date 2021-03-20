EMACS = emacs
CHEZMOI_DIR = $(HOME)/.local/share/chezmoi
EMACS_DIR = $(HOME)/.emacs.d

$(EMACS_DIR)/init.el: $(CHEZMOI_DIR)/dot_emacs.d/init.org
	emacs --batch \
		--eval "(require 'org)" \
		--eval '(org-babel-tangle-file "$(CHEZMOI_DIR)/dot_emacs.d/init.org")'
	mv $(CHEZMOI_DIR)/dot_emacs.d/init.el $(EMACS_DIR)/init.el
