##
## EPITECH PROJECT, 2019
## cpp_rush3_2019
## File description:
## automated desc ftw
##

NAME = koak
SRC = $(shell find ./app ./src -name '*.hs')

$(NAME): all

all: $(SRC)
	stack install koak:exe:koak-exe --local-bin-path '.' --allow-different-user
	mv koak-exe koak -f

tests_run:
	stack test

clean: fclean

fclean:
	rm -rf .stack-work tags TAGS
	find . -name '*.hi' -type f -delete
	find . -name '*.o' -type f -delete

re:
	$(MAKE) fclean
	$(MAKE)

.PHONY: clean re all fclean
