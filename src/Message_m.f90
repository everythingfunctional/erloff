module Message_m
    use Call_stack_m, only: CallStack_t
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type, public :: MessageType_t
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

    character(len=*), parameter :: DEBUG_TYPE_STRING = "Debug_t"
    character(len=*), parameter :: INFO_TYPE_STRING = "Info_t"

    type(MessageType_t), parameter, public :: DEBUG_TYPE = MessageType_t( &
            DEBUG_TYPE_STRING)
    type(MessageType_t), parameter, public :: INFO_TYPE = MessageType_t( &
            INFO_TYPE_STRING)

    interface Info
        module procedure genericInfoCCC
        module procedure genericInfoCCS
        module procedure genericInfoCSC
        module procedure genericInfoCSS
        module procedure genericInfoSCC
        module procedure genericInfoSCS
        module procedure genericInfoSSC
        module procedure genericInfoSSS
        module procedure infoWithTypeCCC
        module procedure infoWithTypeCCS
        module procedure infoWithTypeCSC
        module procedure infoWithTypeCSS
        module procedure infoWithTypeSCC
        module procedure infoWithTypeSCS
        module procedure infoWithTypeSSC
        module procedure infoWithTypeSSS
    end interface Info

    public :: Info
contains
    pure function genericInfoCCC( &
            module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = INFO_TYPE
    end function genericInfoCCC

    pure function genericInfoCCS( &
            module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = INFO_TYPE
    end function genericInfoCCS

    pure function genericInfoCSC( &
            module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = INFO_TYPE
    end function genericInfoCSC

    pure function genericInfoCSS( &
            module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = INFO_TYPE
    end function genericInfoCSS

    pure function genericInfoSCC( &
            module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = INFO_TYPE
    end function genericInfoSCC

    pure function genericInfoSCS( &
            module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = INFO_TYPE
    end function genericInfoSCS

    pure function genericInfoSSC( &
            module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = INFO_TYPE
    end function genericInfoSSC

    pure function genericInfoSSS( &
            module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = INFO_TYPE
    end function genericInfoSSS

    pure function infoWithTypeCCC( &
            type_tag, module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = type_tag
    end function infoWithTypeCCC

    pure function infoWithTypeCCS( &
            type_tag, module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = type_tag
    end function infoWithTypeCCS

    pure function infoWithTypeCSC( &
            type_tag, module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = type_tag
    end function infoWithTypeCSC

    pure function infoWithTypeCSS( &
            type_tag, module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = type_tag
    end function infoWithTypeCSS

    pure function infoWithTypeSCC( &
            type_tag, module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = type_tag
    end function infoWithTypeSCC

    pure function infoWithTypeSCS( &
            type_tag, module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = type_tag
    end function infoWithTypeSCS

    pure function infoWithTypeSSC( &
            type_tag, module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = type_tag
    end function infoWithTypeSSC

    pure function infoWithTypeSSS( &
            type_tag, module_name, procedure_name, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%call_stack = CallStack(module_name, procedure_name)
        info_%message = message
        info_%message_type = type_tag
    end function infoWithTypeSSS

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
        case (INFO_TYPE_STRING)
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
        case (INFO_TYPE_STRING, DEBUG_TYPE_STRING)
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

        string = INFO_TYPE_STRING
    end function infoTypeRepr
end module Message_m
