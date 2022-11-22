## Make the parser and scanner
To make both the parser and the semantic checker run
```
make all
```
To only compile the parser run
```
make test_parser.native
```
To only compile the semantic checker run
```
make test_semantic.native
```
## Test cases
```
bash run_tests.sh
```
The output of each individual test would be under the `log` directory. The cumulative output would be in the file `log/all.log`
