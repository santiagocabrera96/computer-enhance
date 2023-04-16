# Intel 8086 dumb decoder/simulator

Dummy intel 8086 decoder and simulator for the [computer enhance course](https://www.computerenhance.com/).

## Decode binary 8086 file
```shell
clj -m decoder resources/listing_0055_challenge_rectangle
```

## Simulate binary 8086 file
```shell
clj -m simulator resources/listing_0055_challenge_rectangle
```

Print ip register in simulation
```shell
clj -m simulator resources/listing_0055_challenge_rectangle print-ip
```

Dump program memory to result.data
```shell
clj -m simulator resources/listing_0055_challenge_rectangle print-ip dump
```

## Run tests
```shell
clj -A:test
```