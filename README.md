# erloff

[![pipeline status](https://gitlab.com/everythingfunctional/erloff/badges/master/pipeline.svg)](https://gitlab.com/everythingfunctional/erloff/commits/master)

Errors and logging for fortran

## Usage

The basic usage is that a procedure can have intent(out) message and/or error
list arguments. If inside that procedure, you would like to log a message or
signal an error, simply append a message to the appropriate list. The caller
of that procedure is then responsible for either dealing with the errors or
messages, or appending them to its own lists.

### Creating a Message

There are 5 types of messages supported. Debug, Info, Warning, Fatal and
Internal messages. The messages all include the module and procedure they
originated from, as well as all modules and procedures they come through on
the way back up the call stack. They also include a message, and optionally a
tag indicating the type of message. Debug messages also contain a level that can
be used for filtering them later. Creating a message for output will typically
look something like the following:

```Fortran
call messages%appendMessage(Info( &
        Module_("Some_m"), Procedure_("some"), "Hello, World!"))
```

Note that an `ErrorList_t` will only hold messages of type Fatal or Internal.

### Dealing With Messages From a Called Procedure

Any messages produced are returned in the argument to the procedure, and **they
don't go anywhere else**, so it is the responsibility of the calling procedure
to deal with them appropriately. Most of the time that procedure will just
return them as part of it's own outputs like so:

```Fortran
call messages%appendMessages( &
        other_messages, Module_("Some_m"), Procedure("some"))
```

It's also possible to interogate and filter the messages. So, at some point
your program you'll probably have something like the following to print them
out.

```Fortran
to_output = messages.ofType.INFO_TYPE
print *, to_output%toString()
```

### Concurrency

This type of error handling and logging system makes it possible to deal with
errors and messages even from within `pure` procedures, and even have
deterministic concurrency. You could basically have something like the following
and still be guarenteed that any messages will always come out in the same order.

```Fortran
do concurrent (i = 1:n)
    call doSomethingExpensive(input(i), output(i), message_lists(i))
end do
do i = 1, n
    call messages%appendMessages( &
                message_lists(i), Module_("Some_m"), Procedure("some"))
end do
```
