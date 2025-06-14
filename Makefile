NAME=main

all: .gitignore
	ocamlopt -o $(NAME) -I srcs srcs/main.ml

.PHONY: all .gitignore

.gitignore:
	@if [ ! -f .gitignore ]; then \
		echo $(NAME) >> .gitignore; \
		echo "srcs/*.cm*" >> .gitignore; \
		echo "srcs/*.o" >> .gitignore; \
		echo ".gitignore" >> .gitignore; \
	fi

	
	