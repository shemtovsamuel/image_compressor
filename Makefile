##
## EPITECH PROJECT, 2022
## imageCompressor
## File description:
## Makefile
##

BINARY_PATH = $(shell stack path --local-install-root)

NAME = imageCompressor

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re:	fclean all

.PHONY: all $(NAME) clean fclean re