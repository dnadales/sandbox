# Snapping Haskell Programs

An example of a Haskell application packed into
a [`snap`](https://www.ubuntu.com/desktop/snappy).

To create this snap make sure `snapcraft` is installed:

```sh
sudo snap install --beta --classic snapcraft 
```

Then create the snap by running:

```sh
snapcraft
```

Install it:

```sh
sudo snap install hello-haskell-stack_0.1_amd64.snap --devmode --dangerous
```

And finally test it:

```sh
> hello-haskell-stack 
A Haskell program says hello from a snap!
```

