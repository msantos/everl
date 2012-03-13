everl is an Erlang library for asynchronously generating socket
notifications using libev. everl lets you watch file descriptors for
events and sends your process a message whenever the socket state changes.


## ABOUT

everl started out as a contribution to procket by Gregory Haskins
(https://github.com/ghaskins) but it was so awesome I decided to move
it into its own repository. The original patch is here:

    https://github.com/msantos/procket/commit/1345e3b4a00ea10af24bd1cd9d52fa1b28de1b2a
    https://github.com/msantos/procket/pull/2

libev was written by Marc Alexander Lehmann and is available from:

    http://software.schmorp.de/pkg/libev.html


## FEATURES


## EXPORTS

    create(FD, Flags) -> {ok, Handle} | {error, posix()}
    
        Types   FD = int()
                Flags = [Flag]
                Flag = read | write
                Handle = resource()

        Returns a handle for watching events on the file descriptor.

    arm(Handle) -> ok

        Types   Handle = resource()

        Initiates notifications on the file descriptor. The watch handle
        must be re-armed after each notification is received.

        The process creating the handle will receive:

            {everl_watcher, FD, Event}

        Where

            FD is the file descriptor associated with the watch handle.

            Event is an integer:

                1 (READ): file descriptor is ready for reading
                2 (WRITE): file descriptor is ready for writing
                3 (RDWR): file descriptor is ready for reading and writing

    disarm(Handle) -> ok

        Types   Handle = resource()

        Disable notifications for the file descriptor and close the
        watch handle.


## USING IT


## TESTING

    make test


## TODO

* Documentation and examples

* Support all of libev 

* API changes
