SRCNAMES=reset.css fonts.css style.css radian.css syntax.css
SRCS=$(addprefix src/, $(SRCNAMES))

all: $(SRCS)
	uglifycss $^ > all.css
