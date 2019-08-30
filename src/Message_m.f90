module Message_m
    use Call_stack_m, only: CallStack_t
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type, public :: MessageType_t
        private
        character(len=100) :: description
    contains
        private
        procedure, public :: toString => messageTypeToString
        procedure, public :: repr => messageTypeRepr
    end type MessageType_t

    type, public, abstract :: Message_t
        private
        type(CallStack_t) :: call_stack
        type(VARYING_STRING) :: message
        type(MessageType_t) :: message_type
    contains
        private
        procedure, public :: toString => messageToString
        procedure(messageToString_), deferred :: typeString
        procedure :: isType
        generic, public :: operator(.isType.) => isType
        procedure, public :: repr => messageRepr
        procedure(messageToString_), deferred :: typeRepr
    end type Message_t

    abstract interface
        pure function messageToString_(self) result(string)
            use iso_varying_string, only: VARYING_STRING
            import Message_t

            class(Message_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function messageToString_
    end interface

    type, public, extends(Message_t) :: Info_t
    contains
        procedure :: typeString => infoTypeString
        procedure :: typeRepr => infoTypeRepr
    end type Info_t

    character(len=*), parameter :: DEBUG_TYPE_REPR = "Debug_t"
    character(len=*), parameter :: INFO_TYPE_REPR = "Info_t"

    type(MessageType_t), parameter, public :: DEBUG_TYPE = MessageType_t( &
            DEBUG_TYPE_REPR)
    type(MessageType_t), parameter, public :: INFO_TYPE = MessageType_t( &
            INFO_TYPE_REPR)

    public :: Info
contains
    pure function Info(module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = INFO_TYPE
    end function Info

    pure function messageToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: NEWLINE

        class(Message_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = &
                self%call_stack%toString() // ":" // NEWLINE &
                // "    " // self%typeString() // self%message_type%toString() &
                // self%message
    end function messageToString

    pure function isType(self, type_tag)
        class(Message_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tag
        logical :: isType

        select case (trim(type_tag%description))
        case (INFO_TYPE_REPR)
            select type (self)
            class is (Info_t)
                isType = .true.
            class default
                isType = .false.
            end select
        case default
            isType = self%message_type%description == type_tag%description
        end select
    end function isType

    pure function messageRepr(self) result(repr)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, NEWLINE

        class(Message_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = &
                'Message(' // NEWLINE &
                // '    type = ' // self%typeRepr() // ',' // NEWLINE &
                // '    call_stack = ' &
                // hangingIndent(self%call_stack%repr(), 4) // ',' // NEWLINE &
                // '    message_type = ' // self%message_type%repr() // ',' // NEWLINE &
                // '    message = "' // self%message // '")'
    end function messageRepr

    pure function messageTypeToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MessageType_t), intent(in) :: self
        type(VARYING_STRING) :: string

        select case (trim(self%description))
        case (INFO_TYPE_REPR, DEBUG_TYPE_REPR)
            string = ""
        case default
            string = trim(self%description) // ": "
        end select
    end function messageTypeToString

    pure function messageTypeRepr(self) result(repr)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MessageType_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = "MessageType(" // trim(self%description) // ")"
    end function messageTypeRepr

    pure function infoTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Info_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self); end associate

        string = "IN: "
    end function infoTypeString

    pure function infoTypeRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Info_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self); end associate

        string = INFO_TYPE_REPR
    end function infoTypeRepr
end module Message_m
