# Docs

## Fetch the tools

### Install python3 and pip

```console
$ sudo apt update
$ sudo apt install python3 python3-pip
```

### Install the dependencies

```console
$ pip install -r requirements.txt
```

### Add python3 to your path

```console
$ export PATH=$PATH:~/.local/bin
```

## Build the docs

```console
$ make html
```
