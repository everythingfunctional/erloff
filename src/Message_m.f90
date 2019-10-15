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
        procedure, public :: prependNames
        procedure, public :: toString => messageToString
        procedure(messageToString_), deferred :: typeString
        procedure :: isType
        generic, public :: operator(.isType.) => isType
        procedure, public :: repr => messageRepr
        procedure(messageToString_), deferred :: typeRepr
    end type Message_t

    type :: DebugLevel_t
        private
        integer :: level
    contains
        private
        procedure :: toString => debugLevelToString
    end type DebugLevel_t

    type, public, extends(Message_t) :: Debug_t
        private
        type(DebugLevel_t) :: level
    contains
        private
        procedure :: typeString => debugTypeString
        procedure :: typeRepr => debugTypeRepr
    end type Debug_t

    type, public, extends(Message_t) :: Info_t
    contains
        private
        procedure :: typeString => infoTypeString
        procedure :: typeRepr => infoTypeRepr
    end type Info_t

    type, public, extends(Message_t) :: Warning_t
    contains
        private
        procedure :: typeString => warningTypeString
        procedure :: typeRepr => warningTypeRepr
    end type Warning_t

    type, public, abstract, extends(Message_t) :: Error_t
    end type Error_t

    type, public, extends(Error_t) :: Fatal_t
    contains
        private
        procedure :: typeString => fatalTypeString
        procedure :: typeRepr => fatalTypeRepr
    end type Fatal_t

    type, public, extends(Error_t) :: Internal_t
    contains
        private
        procedure :: typeString => internalTypeString
        procedure :: typeRepr => internalTypeRepr
    end type Internal_t

    abstract interface
        function messageToString_(self) result(string)
            use iso_varying_string, only: VARYING_STRING
            import Message_t
            class(Message_t), intent(in) :: self
            type(VARYING_STRING) :: string
        end function messageToString_
    end interface

    interface Debug
        module procedure genericDebugC
        module procedure genericDebugS
        module procedure debugWithTypeC
        module procedure debugWithTypeS
    end interface Debug

    interface Info
        module procedure genericInfoC
        module procedure genericInfoS
        module procedure infoWithTypeC
        module procedure infoWithTypeS
    end interface Info

    interface Warning
        module procedure genericWarningC
        module procedure genericWarningS
        module procedure warningWithTypeC
        module procedure warningWithTypeS
    end interface Warning

    interface Fatal
        module procedure genericFatalC
        module procedure genericFatalS
        module procedure fatalWithTypeC
        module procedure fatalWithTypeS
    end interface Fatal

    interface Internal
        module procedure genericInternalC
        module procedure genericInternalS
        module procedure internalWithTypeC
        module procedure internalWithTypeS
    end interface Internal

    character(len=*), parameter :: DEBUG_TYPE_STRING = "Debug_t"
    character(len=*), parameter :: INFO_TYPE_STRING = "Info_t"
    character(len=*), parameter :: WARNING_TYPE_STRING = "Warning_t"
    character(len=*), parameter :: ERROR_TYPE_STRING = "Error_t"
    character(len=*), parameter :: FATAL_TYPE_STRING = "Fatal_t"
    character(len=*), parameter :: INTERNAL_TYPE_STRING = "Internal_t"

    type(MessageType_t), parameter, public :: DEBUG_TYPE = MessageType_t( &
            DEBUG_TYPE_STRING)
    type(MessageType_t), parameter, public :: INFO_TYPE = MessageType_t( &
            INFO_TYPE_STRING)
    type(MessageType_t), parameter, public :: WARNING_TYPE = MessageType_t( &
            WARNING_TYPE_STRING)
    type(MessageType_t), parameter, public :: ERROR_TYPE = MessageType_t( &
            ERROR_TYPE_STRING)
    type(MessageType_t), parameter, public :: FATAL_TYPE = MessageType_t( &
            FATAL_TYPE_STRING)
    type(MessageType_t), parameter, public :: INTERNAL_TYPE = MessageType_t( &
            INTERNAL_TYPE_STRING)
    type(MessageType_t), parameter, public :: INPUTS_TYPE = MessageType_t( &
            "Inputs")
    type(MessageType_t), parameter, public :: OUTPUTS_TYPE = MessageType_t( &
            "Outputs")
    type(MessageType_t), parameter, public :: OUTSIDE_NORMAL_RANGE_TYPE = &
            MessageType_t("Outside Normal Range")
    type(MessageType_t), parameter, public :: UNEQUAL_ARRAY_SIZES_TYPE = &
            MessageType_t("Unequal Array Sizes")
    type(MessageType_t), parameter, public :: UNKNOWN_TYPE_TYPE = &
            MessageType_t("Unknown Type Encountered")

    type(DebugLevel_t), public, parameter :: GENERAL = DebugLevel_t(1)
    type(DebugLevel_t), public, parameter :: MEDIUM = DebugLevel_t(2)
    type(DebugLevel_t), public, parameter :: DETAILED = DebugLevel_t(3)
    type(DebugLevel_t), public, parameter :: NITTY_GRITTY = DebugLevel_t(4)

    public :: Debug, Info, Warning, Fatal, Internal
contains
    function genericDebugC(module_, procedure_, level, message) result(debug_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_ = Debug(DEBUG_TYPE, module_, procedure_, level, var_str(message))
    end function genericDebugC

    function genericDebugS(module_, procedure_, level, message) result(debug_)
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_ = Debug(DEBUG_TYPE, module_, procedure_, level, message)
    end function genericDebugS

    function debugWithTypeC( &
            type_tag, module_, procedure_, level, message) result(debug_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_ = Debug(type_tag, module_, procedure_, level, var_str(message))
    end function debugWithTypeC

    function debugWithTypeS( &
            type_tag, module_, procedure_, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%message_type = type_tag
        debug_%call_stack = CallStack(module_, procedure_)
        debug_%level = level
        debug_%message = message
    end function debugWithTypeS

    function genericInfoC(module_, procedure_, message) result(info_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_ = Info(INFO_TYPE, module_, procedure_, var_str(message))
    end function genericInfoC

    function genericInfoS(module_, procedure_, message) result(info_)
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_ = Info(INFO_TYPE, module_, procedure_, message)
    end function genericInfoS

    function infoWithTypeC(type_tag, module_, procedure_, message) result(info_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_ = Info(type_tag, module_, procedure_, var_str(message))
    end function infoWithTypeC

    function infoWithTypeS(type_tag, module_, procedure_, message) result(info_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%message_type = type_tag
        info_%call_stack = CallStack(module_, procedure_)
        info_%message = message
    end function infoWithTypeS

    function genericWarningC(module_, procedure_, message) result(warning_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_ = Warning(WARNING_TYPE, module_, procedure_, var_str(message))
    end function genericWarningC

    function genericWarningS(module_, procedure_, message) result(warning_)
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_ = Warning(WARNING_TYPE, module_, procedure_, message)
    end function genericWarningS

    function warningWithTypeC(type_tag, module_, procedure_, message) result(warning_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_ = Warning(type_tag, module_, procedure_, var_str(message))
    end function warningWithTypeC

    function warningWithTypeS(type_tag, module_, procedure_, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%message_type = type_tag
        warning_%call_stack = CallStack(module_, procedure_)
        warning_%message = message
    end function warningWithTypeS

    function genericFatalC(module_, procedure_, message) result(fatal_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_ = Fatal(FATAL_TYPE, module_, procedure_, var_str(message))
    end function genericFatalC

    function genericFatalS(module_, procedure_, message) result(fatal_)
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_ = Fatal(FATAL_TYPE, module_, procedure_, message)
    end function genericFatalS

    function fatalWithTypeC(type_tag, module_, procedure_, message) result(fatal_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_ = Fatal(type_tag, module_, procedure_, var_str(message))
    end function fatalWithTypeC

    function fatalWithTypeS(type_tag, module_, procedure_, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%message_type = type_tag
        fatal_%call_stack = CallStack(module_, procedure_)
        fatal_%message = message
    end function fatalWithTypeS

    function genericInternalC(module_, procedure_, message) result(internal_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_ = Internal(INTERNAL_TYPE, module_, procedure_, var_str(message))
    end function genericInternalC

    function genericInternalS(module_, procedure_, message) result(internal_)
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_ = Internal(INTERNAL_TYPE, module_, procedure_, message)
    end function genericInternalS

    function internalWithTypeC(type_tag, module_, procedure_, message) result(internal_)
        use iso_varying_string, only: var_str
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_ = Internal(type_tag, module_, procedure_, var_str(message))
    end function internalWithTypeC

    function internalWithTypeS(type_tag, module_, procedure_, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%message_type = type_tag
        internal_%call_stack = CallStack(module_, procedure_)
        internal_%message = message
    end function internalWithTypeS

    function messageTypeToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MessageType_t), intent(in) :: self
        type(VARYING_STRING) :: string

        select case (trim(self%description))
        case ( &
                DEBUG_TYPE_STRING, &
                INFO_TYPE_STRING, &
                WARNING_TYPE_STRING, &
                ERROR_TYPE_STRING, &
                FATAL_TYPE_STRING, &
                INTERNAL_TYPE_STRING)
            string = ""
        case default
            string = trim(self%description) // ": "
        end select
    end function messageTypeToString

    function messageTypeRepr(self) result(repr)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MessageType_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = "MessageType(" // trim(self%description) // ")"
    end function messageTypeRepr

    subroutine prependNames(self, module_, procedure_)
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        class(Message_t), intent(inout) :: self
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_

        call self%call_stack%prependNames(module_, procedure_)
    end subroutine prependNames

    function messageToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, NEWLINE

        class(Message_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hangingIndent( &
                self%call_stack%toString() // ":" // NEWLINE &
                    // self%typeString() // self%message_type%toString() &
                    // self%message, &
                4)
    end function messageToString

    function isType(self, type_tag)
        class(Message_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tag
        logical :: isType

        select case (trim(type_tag%description))
        case (DEBUG_TYPE_STRING)
            select type (self)
            class is (Debug_t)
                isType = .true.
            class default
                isType = .false.
            end select
        case (INFO_TYPE_STRING)
            select type (self)
            class is (Info_t)
                isType = .true.
            class default
                isType = .false.
            end select
        case (WARNING_TYPE_STRING)
            select type (self)
            class is (Warning_t)
                isType = .true.
            class default
                isType = .false.
            end select
        case (ERROR_TYPE_STRING)
            select type (self)
            class is (Error_t)
                isType = .true.
            class default
                isType = .false.
            end select
        case (FATAL_TYPE_STRING)
            select type (self)
            class is (Fatal_t)
                isType = .true.
            class default
                isType = .false.
            end select
        case (INTERNAL_TYPE_STRING)
            select type (self)
            class is (Internal_t)
                isType = .true.
            class default
                isType = .false.
            end select
        case default
            isType = self%message_type%description == type_tag%description
        end select
    end function isType

    function messageRepr(self) result(repr)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, NEWLINE

        class(Message_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = hangingIndent( &
                'Message(' // NEWLINE &
                    // 'type = ' // self%typeRepr() // ',' // NEWLINE &
                    // 'call_stack = ' // self%call_stack%repr() // ',' // NEWLINE &
                    // 'message_type = ' // self%message_type%repr() // ',' // NEWLINE &
                    // 'message = "' // self%message // '"', &
                4) // NEWLINE // ')'
    end function messageRepr

    function debugLevelToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        class(DebugLevel_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = toString(self%level)
    end function debugLevelToString

    function debugTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(Debug_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = "DB-" // self%level%toString() // ": "
    end function debugTypeString

    function debugTypeRepr(self) result(repr)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(Debug_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = DEBUG_TYPE_STRING // '(level = ' // self%level%toString() // ')'
    end function debugTypeRepr

    function infoTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Info_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self)
        end associate

        string = "IN: "
    end function infoTypeString

    function infoTypeRepr(self) result(repr)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Info_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        associate(a => self)
        end associate

        repr = INFO_TYPE_STRING
    end function infoTypeRepr

    function warningTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Warning_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self)
        end associate

        string = "WN: "
    end function warningTypeString

    function warningTypeRepr(self) result(repr)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Warning_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        associate(a => self)
        end associate

        repr = WARNING_TYPE_STRING
    end function warningTypeRepr

    function fatalTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Fatal_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self)
        end associate

        string = "FE: "
    end function fatalTypeString

    function fatalTypeRepr(self) result(repr)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Fatal_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        associate(a => self)
        end associate

        repr = FATAL_TYPE_STRING
    end function fatalTypeRepr

    function internalTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Internal_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self)
        end associate

        string = "IE: "
    end function internalTypeString

    function internalTypeRepr(self) result(repr)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Internal_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        associate(a => self)
        end associate

        repr = INTERNAL_TYPE_STRING
    end function internalTypeRepr
end module Message_m
