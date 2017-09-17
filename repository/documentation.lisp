#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.repository)

(docs:define-docs
  (variable *repository-types*
    "List holding the available types of repositories to use with CREATE.

See CREATE")
    
  (type remote-ish
    "Class representing an object tied to a remote location somehow.

See PERMALINK")

  (function permalink
    "Returns a URL string that represents the object's location at its remote location.

See REMOTE-ISH")

  (type located
    "Class representing an object tied to a file-system location.

See LOCATION
See DELETE")

  (function location
    "Returns a pathname of the location the object is tied to.

See LOCATED")

  (function delete
    "Deletes the file system resources represented by the object.

This always returns the object that was passed as argument.
If the deletion should not be possible for some reason, an error
is signalled.

See LOCATED")

  (function create
    "Create a new copy of the given repository.

TYPE      --- A keyword naming the type of repository to create.
LOCATION  --- A pathname for the location on disk to store the
              repository files in.
REMOTE    --- Some kind of object that represents a remote repository
              to clone from.
BRANCH    --- The branch of the repository to clone. This might not
              have an effect for types that don't have branches.

Returns a REPOSITORY instance.

See REPOSITORY")

  (type repository
    "Class representing a repository.

A repository is made up of one or more branches, which are each
made up of a sequence of commits. Repositories mirror a remote
repository and must be periodically updated to reflect the remote
structure.

See REMOTE-ISH
See LOCATED
See UPDATE
See LIST-COMMITS
See FIND-COMMIT")

  (function update
    "Synchronise the repository with its remote.

This will load in new commits if there are any. If the remote is
somewhere on the network, an internet connection will be used.

This always returns the object that was passed as argument.
If the update should not be possible for some reason, an error
is signalled.

See REPOSITORY")

  (function list-commits
    "Return a list of commit instances for the repository.

The commit instances are persistent-- meaning the same commits are
guaranteed to be represented by the same object across multiple
calls to this function.

The commits are ordered in descending order of their timestamp.

See REPOSITORY
See COMMIT")

  (function find-commit
    "Find a particular commit in the repository.

If ERROR is non-NIL and a commit with the given ID cannot be found
on the repository, an error is signalled.

See REPOSITORY
See COMMIT")

  (type commit
    "Class representing a commit on a repository.

A commit is a bundle of changes made to a set of files.
A commit can be signed off by an author at a particular time, with
an accompanying message describing the contents of the commit.
Each commit must have a unique ID string that identifies the commit
in the repository.

See REMOTE-ISH
See REPOSITORY
See ID
See AUTHOR
See MESSAGE
See TIMESTAMP
See CHECKOUT")

  (function repository
    "Returns the repository instance the commit is a part of.

See COMMIT
See REPOSITORY")

  (function id
    "Returns the ID string that uniquely identifies the commit in its repository.

See COMMIT")

  (function author
    "Returns a string describing the author of the commit, or NIL.

See COMMIT")

  (function message
    "Returns a string describing the contents of the commit, or NIL.

See COMMIT")

  (function timestamp
    "Returns a universal-time timestamp describing the time the commit was made, or NIL.

See COMMIT")

  (function checkout
    "Check out the repository's state at a particular commit to a location.

This will cause the location to be populated in such a way as to
contain all files in the given state they were at after the given
commit was made to the repository.

See COMMIT
See CHECKOUT")

  (type checkout
    "Class representing a checked-out commit of a repository.

See COMMIT
See LOCATED
See CLEAN")

  (function clean
    "Undo all changes made to the files at the checkout's location.

This will restore the files to the state they were in when the
original checkout was made.

This always returns the object that was passed as argument.
If the cleaning should not be possible for some reason, an error
is signalled.

See CHECKOUT"))
