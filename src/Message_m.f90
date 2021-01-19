module Message_m
    use Call_stack_m, only: CallStack_t, CallStack
    use iso_varying_string, only: &
            VARYING_STRING, assignment(=), operator(//), var_str
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: Procedure_t
    use strff, only: operator(.includes.), hanging_indent, to_string, NEWLINE

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
        procedure :: originatedFromModule
        procedure :: originatedFromProcedure
        generic, public :: operator(.originatedFrom.) => &
                originatedFromModule, originatedFromProcedure
        procedure :: isFromModule
        procedure :: isFromProcedure
        generic, public :: operator(.isFrom.) => &
                isFromModule, isFromProcedure
        procedure :: cameThroughModule
        procedure :: cameThroughProcedure
        generic, public :: operator(.cameThrough.) => &
                cameThroughModule, cameThroughProcedure
        procedure :: includesC
        procedure :: includesS
        generic, public :: operator(.includes.) => &
                includesC, includesS
        procedure :: includesAnyOf
        generic, public :: operator(.includesAnyOf.) => includesAnyOf
        procedure :: includesAllOf
        generic, public :: operator(.includesAllOf.) => includesAllOf
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
        pure function messageToString_(self) result(string)
            import Message_t, VARYING_STRING
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
    type(MessageType_t), parameter, public :: NOT_FOUND_TYPE = MessageType_t( &
            "Not Found")
    type(MessageType_t), parameter, public :: OUT_OF_BOUNDS_TYPE = MessageType_t( &
            "Out of Bounds")
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
    pure function genericDebugC(module_, procedure_, level, message) result(debug_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_ = Debug(DEBUG_TYPE, module_, procedure_, level, var_str(message))
    end function genericDebugC

    pure function genericDebugS(module_, procedure_, level, message) result(debug_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_ = Debug(DEBUG_TYPE, module_, procedure_, level, message)
    end function genericDebugS

    pure function debugWithTypeC( &
            type_tag, module_, procedure_, level, message) result(debug_)
        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_ = Debug(type_tag, module_, procedure_, level, var_str(message))
    end function debugWithTypeC

    pure function debugWithTypeS( &
            type_tag, module_, procedure_, level, message) result(debug_)
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

    pure function genericInfoC(module_, procedure_, message) result(info_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_ = Info(INFO_TYPE, module_, procedure_, var_str(message))
    end function genericInfoC

    pure function genericInfoS(module_, procedure_, message) result(info_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_ = Info(INFO_TYPE, module_, procedure_, message)
    end function genericInfoS

    pure function infoWithTypeC(type_tag, module_, procedure_, message) result(info_)
        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_ = Info(type_tag, module_, procedure_, var_str(message))
    end function infoWithTypeC

    pure function infoWithTypeS(type_tag, module_, procedure_, message) result(info_)
        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%message_type = type_tag
        info_%call_stack = CallStack(module_, procedure_)
        info_%message = message
    end function infoWithTypeS

    pure function genericWarningC(module_, procedure_, message) result(warning_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_ = Warning(WARNING_TYPE, module_, procedure_, var_str(message))
    end function genericWarningC

    pure function genericWarningS(module_, procedure_, message) result(warning_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_ = Warning(WARNING_TYPE, module_, procedure_, message)
    end function genericWarningS

    pure function warningWithTypeC(type_tag, module_, procedure_, message) result(warning_)
        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_ = Warning(type_tag, module_, procedure_, var_str(message))
    end function warningWithTypeC

    pure function warningWithTypeS(type_tag, module_, procedure_, message) result(warning_)
        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%message_type = type_tag
        warning_%call_stack = CallStack(module_, procedure_)
        warning_%message = message
    end function warningWithTypeS

    pure function genericFatalC(module_, procedure_, message) result(fatal_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_ = Fatal(FATAL_TYPE, module_, procedure_, var_str(message))
    end function genericFatalC

    pure function genericFatalS(module_, procedure_, message) result(fatal_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_ = Fatal(FATAL_TYPE, module_, procedure_, message)
    end function genericFatalS

    pure function fatalWithTypeC(type_tag, module_, procedure_, message) result(fatal_)
        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_ = Fatal(type_tag, module_, procedure_, var_str(message))
    end function fatalWithTypeC

    pure function fatalWithTypeS(type_tag, module_, procedure_, message) result(fatal_)
        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%message_type = type_tag
        fatal_%call_stack = CallStack(module_, procedure_)
        fatal_%message = message
    end function fatalWithTypeS

    pure function genericInternalC(module_, procedure_, message) result(internal_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_ = Internal(INTERNAL_TYPE, module_, procedure_, var_str(message))
    end function genericInternalC

    pure function genericInternalS(module_, procedure_, message) result(internal_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_ = Internal(INTERNAL_TYPE, module_, procedure_, message)
    end function genericInternalS

    pure function internalWithTypeC(type_tag, module_, procedure_, message) result(internal_)
        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_ = Internal(type_tag, module_, procedure_, var_str(message))
    end function internalWithTypeC

    pure function internalWithTypeS(type_tag, module_, procedure_, message) result(internal_)
        type(MessageType_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%message_type = type_tag
        internal_%call_stack = CallStack(module_, procedure_)
        internal_%message = message
    end function internalWithTypeS

    pure function messageTypeToString(self) result(string)
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

    pure function messageTypeRepr(self) result(repr)
        class(MessageType_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = "MessageType(" // trim(self%description) // ")"
    end function messageTypeRepr

    pure subroutine prependNames(self, module_, procedure_)
        class(Message_t), intent(inout) :: self
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_

        call self%call_stack%prependNames(module_, procedure_)
    end subroutine prependNames

    pure function messageToString(self) result(string)
        class(Message_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = hanging_indent( &
                self%call_stack%toString() // ":" // NEWLINE &
                    // self%typeString() // self%message_type%toString() &
                    // self%message, &
                4)
    end function messageToString

    pure function isType(self, type_tag)
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

    pure function originatedFromModule(self, module_) result(originated_from)
        class(Message_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: originated_from

        originated_from = self%call_stack.originatedFrom.module_
    end function originatedFromModule

    pure function originatedFromProcedure(self, procedure_) result(originated_from)
        class(Message_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: originated_from

        originated_from = self%call_stack.originatedFrom.procedure_
    end function originatedFromProcedure

    pure function isFromModule(self, module_) result(is_from)
        class(Message_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: is_from

        is_from = self%call_stack.includes.module_
    end function isFromModule

    pure function isFromProcedure(self, procedure_) result(is_from)
        class(Message_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: is_from

        is_from = self%call_stack.includes.procedure_
    end function isFromProcedure

    pure function cameThroughModule(self, module_) result(came_through)
        class(Message_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: came_through

        came_through = &
                (.not.(self.originatedFrom.module_)) &
                .and.(self.isFrom.module_)
    end function cameThroughModule

    pure function cameThroughProcedure(self, procedure_) result(came_through)
        class(Message_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: came_through

        came_through = &
                (.not.(self.originatedFrom.procedure_)) &
                .and.(self.isFrom.procedure_)
    end function cameThroughProcedure

    pure function includesC(self, string) result(includes)
        class(Message_t), intent(in) :: self
        character(len=*), intent(in) :: string
        logical :: includes

        includes = self.includes.var_str(string)
    end function includesC

    pure function includesS(self, string) result(includes)
        class(Message_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        logical :: includes

        includes = self%message.includes.string
    end function includesS

    pure function includesAnyOf(self, strings) result(includes)
        class(Message_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        logical :: includes

        integer :: i
        logical :: includes_(size(strings))

        do i = 1, size(strings)
            includes_(i) = self.includes.strings(i)
        end do
        includes = any(includes_)
    end function includesAnyOf

    pure function includesAllOf(self, strings) result(includes)
        class(Message_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        logical :: includes

        integer :: i
        logical :: includes_(size(strings))

        do i = 1, size(strings)
            includes_(i) = self.includes.strings(i)
        end do
        includes = all(includes_)
    end function includesAllOf

    pure function messageRepr(self) result(repr)
        class(Message_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = hanging_indent( &
                'Message(' // NEWLINE &
                    // 'type = ' // self%typeRepr() // ',' // NEWLINE &
                    // 'call_stack = ' // self%call_stack%repr() // ',' // NEWLINE &
                    // 'message_type = ' // self%message_type%repr() // ',' // NEWLINE &
                    // 'message = "' // self%message // '"', &
                4) // NEWLINE // ')'
    end function messageRepr

    pure function debugLevelToString(self) result(string)
        class(DebugLevel_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = to_string(self%level)
    end function debugLevelToString

    pure function debugTypeString(self) result(string)
        class(Debug_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = "DB-" // self%level%toString() // ": "
    end function debugTypeString

    pure function debugTypeRepr(self) result(repr)
        class(Debug_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = DEBUG_TYPE_STRING // '(level = ' // self%level%toString() // ')'
    end function debugTypeRepr

    pure function infoTypeString(self) result(string)
        class(Info_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self)
        end associate

        string = "IN: "
    end function infoTypeString

    pure function infoTypeRepr(self) result(repr)
        class(Info_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        associate(a => self)
        end associate

        repr = INFO_TYPE_STRING
    end function infoTypeRepr

    pure function warningTypeString(self) result(string)
        class(Warning_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self)
        end associate

        string = "WN: "
    end function warningTypeString

    pure function warningTypeRepr(self) result(repr)
        class(Warning_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        associate(a => self)
        end associate

        repr = WARNING_TYPE_STRING
    end function warningTypeRepr

    pure function fatalTypeString(self) result(string)
        class(Fatal_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self)
        end associate

        string = "FE: "
    end function fatalTypeString

    pure function fatalTypeRepr(self) result(repr)
        class(Fatal_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        associate(a => self)
        end associate

        repr = FATAL_TYPE_STRING
    end function fatalTypeRepr

    pure function internalTypeString(self) result(string)
        class(Internal_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self)
        end associate

        string = "IE: "
    end function internalTypeString

    pure function internalTypeRepr(self) result(repr)
        class(Internal_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        associate(a => self)
        end associate

        repr = INTERNAL_TYPE_STRING
    end function internalTypeRepr
end module Message_m
