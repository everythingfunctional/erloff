# erloff

[![pipeline status](https://gitlab.com/everythingfunctional/erloff/badges/main/pipeline.svg)](https://gitlab.com/everythingfunctional/erloff/commits/main)

Errors and logging for fortran

## Usage

The basic usage is that a procedure can have intent(out) message and/or error list arguments, or as a component of it's return value.
If inside that procedure, you would like to log a message or signal an error, simply append a message to the appropriate list.
The caller of that procedure is then responsible for either dealing with the errors or messages, or appending them to its own lists.

### Creating a Message

There are 5 types of messages supported.
Debug, info, warning, fatal and internal messages.
The messages all include the module and procedure they originated from,
as well as all modules and procedures they come through on the way back up the call stack.
They also include a message, and optionally a tag indicating the type of message.
Debug messages also contain a level that can be used for filtering them later.
Creating a message for output will typically look something like the following:

```Fortran
messages = messages%with_message_appended(info_t( &
        module_t("some_m"), procedure_t("some"), "Hello, World!"))
```

Note that an `error_list_t` will only hold messages of type fatal or internal.

### Dealing With Messages From a Called Procedure

Any messages produced should be returned from the procedure, and **they don't go anywhere else**,
so it is the responsibility of the calling procedure to deal with them appropriately.
Most of the time that procedure will just return them as part of it's own outputs like so:

```Fortran
messages = messages%with_messages_appended( &
        other_messages, module_t("some_m"), procedure_t("some"))
```

It's also possible to interrogate and filter the messages.
So, at some point in your program you'll probably have something like the following to print them out.

```Fortran
to_output = messages.ofType.INFO_TYPE
call put_line(to_output%to_string())
```

### Concurrency

This type of error handling and logging system makes it possible to deal with errors and messages and have deterministic concurrency.
You could basically have something like the following and still be guaranteed that any messages will always come out in the same order.

```Fortran
integer :: input(n), output(n)
type(message_list_t) :: message_lists(n)

call do_something_expensive_elementally(input, output, message_lists)
do i = 1, n
    messages = messages%with_messages_appended( &
            message_lists(i), module_t("some_m"), procedure_t("some"))
end do
```
