# landlock-hs: Haskell bindings for the Linux Landlock API

This library exposes Haskell bindings for the Linux kernel Landlock API.

The Linux kernel Landlock API provides unprivileged access control. The goal
of Landlock is to enable to restrict ambient rights (e.g. global filesystem
access) for a set of processes. Because Landlock is a stackable LSM, it makes
possible to create safe security sandboxes as new security layers in addition
to the existing system-wide access-controls. This kind of sandbox is expected
to help mitigate the security impact of bugs or unexpected/malicious
behaviors in user space applications. Landlock empowers any process,
including unprivileged ones, to securely restrict themselves.

For more information, see the [Landlock homepage](https://landlock.io/) and its
[kernel documentation](https://docs.kernel.org/userspace-api/landlock.html).
