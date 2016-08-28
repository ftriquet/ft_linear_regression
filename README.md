# linear-regression

Clojure linear regression implementation

## Installation

```
git clone https://github.com/ftriquet/ft_linear_regression.git
```
```
lein uberjar
```

## Usage

    $ java -jar target/uberjar/linear-regression-0.1.0-standalone.jar [args]

## Options

--predict: Prompt for values and outputs the predictions based on previously generated model

--file FILE: specifies the dataset file. The file must be csv formated

--train: Start training on a dataset specified by the FILE option

--graph: Display the calculated graph if the regression has one parameter

--animation: Display an animation of the graph fitting the dataset if the regression has one parameter


## Examples

    $ java -jar target/uberjar/linear-regression-0.1.0-standalone.jar --train --animation --file data.csv
