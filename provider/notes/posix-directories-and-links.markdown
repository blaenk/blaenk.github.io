---
title: POSIX Directories and Links
published: March 6, 2013
excerpt: Directories and Links in the POSIX standards
comments: off
---

* toc

Notes for chapter 18 concerning directories and links.

## Directories

Stored in the file system as a regular file except they are marked differently in their i-node entry and their data content is a table mapping filenames to i-node numbers.

## Hard Links

Hard links are also referred to simply as 'links'. It is soft links which require additional qualification. Links can be created using the `ln` command which is often used to instead create soft (symbolic) links (`ln -s`). Creating a hard link to a file doesn't copy (`cp`) the file itself, instead it creates a different filename pointing to the same i-node number and also increases the 'link count' of the file. This can be verified by running `ls -li` to see each file's corresponding i-node number and link count.

Hard links can't be made to directories, thereby preventing circular links. The book recounts how early UNIX implementations did allow this in order to facilitate directory creation. [`mkdir`](http://man7.org/linux/man-pages/man2/mkdir.2.html) didn't exist, so directories were created with [`mknod`](http://man7.org/linux/man-pages/man2/mknod.2.html) and then links were made for `.` and `..` to facilitate directory traversal. It also reminds the reader that 'links to directories' are more or less possible with bind mounts.

### Shared Pointers

Hard links remind me of [`shared_ptr`](http://en.cppreference.com/w/cpp/memory/shared_ptr) in C++11. I can imagine a scenario in which different processes need access to a common file but the common file needs to be deleted when all processes are finished with it. They can create a link to the file and use that to do their work, since it will be the same file as the original. When they are finished with the file, they can [`unlink`](http://man7.org/linux/man-pages/man2/unlink.2.html) (i.e. remove the link) to the file. The file system will automatically delete the file itself when the number of links has reached zero. I don't know if this is common---or even a correct---practice, nevertheless I immediately thought of this when I came across links.

### Temporary Files

A trick in the spirit of the above is touched upon by the book. It talks about how a program might sometimes create a file, `unlink` it immediately, and then continue using the file knowing that the file will be destroyed 1) explicitly when the file descriptor is closed or 2) implicitly when the program closes. This is what [`tmpfile`](http://man7.org/linux/man-pages/man3/tmpfile.3.html) does.

## Symbolic Links

Also known as soft links, these types of links are more commonly used by people. They simply consist of the `type` i-node field being set to `symlink` and the data blocks of the i-node set to the target path.

An interesting note discussed by the book is that some UNIX file systems (such as ext2, ext3, and ext4) perform an optimization where, if the target path can fit in the part of the i-node that would normally be used for data-block pointers, the path is simply stored there instead of externally. In the case of the author, the ext filesystems appropriate 60 bytes to the data-block pointers. Analysis of his system showed that of the 20,070 symbolic links, 97% were 60 bytes or smaller.

## Directory Streams

Directory entries can be enumerated by getting a directory stream handle with [`opendir`](http://man7.org/linux/man-pages/man3/opendir.3.html) (or `fdopendir` to avoid certain race conditions) and pulling directory entries `dirent` from the directory stream with [`readdir`](http://man7.org/linux/man-pages/man2/readdir.2.html).

Additionally, recursive file tree walking can be achieved using [`nftw`](http://man7.org/linux/man-pages/man3/ftw.3.html) (new file tree walking) by passing it a callback to call on every entry.

## Working Directories

The working directory ([`getcwd`](http://man7.org/linux/man-pages/man3/getcwd.3.html)) of a process determines the reference point from which to resolve relative pathnames within the process. For example if the working directory is `/home/user` then a a file path of `../user2` will refer to `/home/user2`. Simple stuff. The working directory can be changed with [`chdir`](http://man7.org/linux/man-pages/man2/chdir.2.html) and `fchdir`.

Aside from this, Linux (> 2.6.16) provides various `*at()` calls, such as [`openat`](http://man7.org/linux/man-pages/man2/openat.2.html), which operate relative to a directory file descriptor. These calls (now part of SuSv4) help avoid certain race conditions and help facilitate an idea of "virtual working directories" which is particularly useful in multithreaded applications since every thread shares the working directory attribute of the process.

## Root Directories

Every process also has a root directory which serves as the reference point from which to resolve _absolute_ pathnames (as opposed to relative pathnames with working directories). This is usually `/`, but can be changed with [`chroot`](http://man7.org/linux/man-pages/man2/chroot.2.html), which is often used to create so called "chroot jails", something FTP servers might do to limit a user's filesystem exposure to their home directory.  One thing to remember to do is to change the working directory to the chrooted path, in effect "stepping into the jail." Otherwise the user is able to continue roaming around outside the jail.

chroot jails aren't a silver bullet. Some BSD derivatives provide a systemcall, [`jail`](http://www.freebsd.org/cgi/man.cgi?query=jail&apropos=0&sektion=2&manpath=FreeBSD+9.1-RELEASE&arch=default&format=html), that handles various edge cases.
