module erloff
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type, public :: Message_t
        private
        type(VARYING_STRING) :: module_name
        type(VARYING_STRING) :: procedure_name
        type(VARYING_STRING) :: message
    end type Message_t

    type, public :: MessageList_t
        private
        type(Message_t), allocatable :: messages(:)
    contains
        private
        procedure, public :: toString
    end type MessageList_t

    public :: Info, MessageList
contains
    pure function Info(module_name, procedure_name, message) result(info_)
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Message_t) :: info_

        info_%module_name = module_name
        info_%procedure_name = procedure_name
        info_%message = message
    end function Info

    pure function MessageList(message) result(message_list)
        type(Message_t), intent(in) :: message
        type(MessageList_t) :: message_list

        allocate(message_list%messages(1))
        message_list%messages(1) = message
    end function MessageList

    pure function toString(message_list) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: NEWLINE

        class(MessageList_t), intent(in) :: message_list
        type(VARYING_STRING) :: string

        string = &
                message_list%messages(1)%module_name // "." &
                // message_list%messages(1)%procedure_name // ":" // NEWLINE &
                // "    IN: " // message_list%messages(1)%message
    end function toString
end module erloff
