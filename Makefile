UBERJAR=target/uberjar/linear-regression-0.1.0-standalone.jar

all: $(UBERJAR) train predict

train: $(UBERJAR)
	@echo "#!/bin/sh" > train
	@echo 'java -jar "$(UBERJAR)" --train $$@' >> train
	@chmod +x train

predict: $(UBERJAR)
	@echo "#!/bin/sh" > predict
	@echo 'java -jar "$(UBERJAR)" --predict $$@' >> predict
	@chmod +x predict

$(UBERJAR): src/linear_regression/matrix.clj src/linear_regression/learning.clj src/linear_regression/core.clj
	lein uberjar

clean:
	rm -f predict
	rm -f train

fclean: clean
	rm -f $(UBERJAR)

re: fclean all

.PHONY: re fclean clean all
