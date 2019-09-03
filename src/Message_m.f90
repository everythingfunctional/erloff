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
        procedure :: prependNamesCC
        procedure :: prependNamesCS
        procedure :: prependNamesSC
        procedure :: prependNamesSS
        generic, public :: prependNames => &
                prependNamesCC, prependNamesCS, prependNamesSC, prependNamesSS
        procedure, public :: toString => messageToString
        procedure(messageToString_), deferred :: typeString
        procedure :: isType
        procedure :: originatedFromProcedureC
        procedure :: originatedFromProcedureS
        generic, public :: operator(.originatedFromProcedure.) => &
                originatedFromProcedureC, originatedFromProcedureS
        procedure :: originatedFromModuleC
        procedure :: originatedFromModuleS
        generic, public :: operator(.originatedFromModule.) => &
                originatedFromModuleC, originatedFromModuleS
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

    type :: DebugLevel_t
        private
        integer :: level
    contains
        private
        procedure :: toString => debugLevelToString
    end type DebugLevel_t

    type(DebugLevel_t), public, parameter :: GENERAL = DebugLevel_t(1)
    type(DebugLevel_t), public, parameter :: MEDIUM = DebugLevel_t(2)
    type(DebugLevel_t), public, parameter :: DETAILED = DebugLevel_t(3)
    type(DebugLevel_t), public, parameter :: NITTY_GRITTY = DebugLevel_t(4)

    type, public, extends(Message_t) :: Debug_t
        private
        type(DebugLevel_t) :: level
    contains
        procedure :: typeString => debugTypeString
        procedure :: typeRepr => debugTypeRepr
    end type Debug_t

    type, public, extends(Message_t) :: Info_t
    contains
        procedure :: typeString => infoTypeString
        procedure :: typeRepr => infoTypeRepr
    end type Info_t

    type, public, extends(Message_t) :: Warning_t
    contains
        procedure :: typeString => warningTypeString
        procedure :: typeRepr => warningTypeRepr
    end type Warning_t

    type, public, abstract, extends(Message_t) :: Error_t
    end type Error_t

    type, public, extends(Error_t) :: Fatal_t
    contains
        procedure :: typeString => fatalTypeString
        procedure :: typeRepr => fatalTypeRepr
    end type Fatal_t

    type, public, extends(Error_t) :: Internal_t
    contains
        procedure :: typeString => internalTypeString
        procedure :: typeRepr => internalTypeRepr
    end type Internal_t

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

    interface Debug
        module procedure genericDebugCCC
        module procedure genericDebugCCS
        module procedure genericDebugCSC
        module procedure genericDebugCSS
        module procedure genericDebugSCC
        module procedure genericDebugSCS
        module procedure genericDebugSSC
        module procedure genericDebugSSS
        module procedure debugWithTypeCCC
        module procedure debugWithTypeCCS
        module procedure debugWithTypeCSC
        module procedure debugWithTypeCSS
        module procedure debugWithTypeSCC
        module procedure debugWithTypeSCS
        module procedure debugWithTypeSSC
        module procedure debugWithTypeSSS
    end interface Debug

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

    interface Warning
        module procedure genericWarningCCC
        module procedure genericWarningCCS
        module procedure genericWarningCSC
        module procedure genericWarningCSS
        module procedure genericWarningSCC
        module procedure genericWarningSCS
        module procedure genericWarningSSC
        module procedure genericWarningSSS
        module procedure warningWithTypeCCC
        module procedure warningWithTypeCCS
        module procedure warningWithTypeCSC
        module procedure warningWithTypeCSS
        module procedure warningWithTypeSCC
        module procedure warningWithTypeSCS
        module procedure warningWithTypeSSC
        module procedure warningWithTypeSSS
    end interface Warning

    interface Fatal
        module procedure genericFatalCCC
        module procedure genericFatalCCS
        module procedure genericFatalCSC
        module procedure genericFatalCSS
        module procedure genericFatalSCC
        module procedure genericFatalSCS
        module procedure genericFatalSSC
        module procedure genericFatalSSS
        module procedure fatalWithTypeCCC
        module procedure fatalWithTypeCCS
        module procedure fatalWithTypeCSC
        module procedure fatalWithTypeCSS
        module procedure fatalWithTypeSCC
        module procedure fatalWithTypeSCS
        module procedure fatalWithTypeSSC
        module procedure fatalWithTypeSSS
    end interface Fatal

    interface Internal
        module procedure genericInternalCCC
        module procedure genericInternalCCS
        module procedure genericInternalCSC
        module procedure genericInternalCSS
        module procedure genericInternalSCC
        module procedure genericInternalSCS
        module procedure genericInternalSSC
        module procedure genericInternalSSS
        module procedure internalWithTypeCCC
        module procedure internalWithTypeCCS
        module procedure internalWithTypeCSC
        module procedure internalWithTypeCSS
        module procedure internalWithTypeSCC
        module procedure internalWithTypeSCS
        module procedure internalWithTypeSSC
        module procedure internalWithTypeSSS
    end interface Internal

    public :: Debug, Info, Warning, Fatal, Internal
contains
    pure function genericDebugCCC( &
            module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = DEBUG_TYPE
    end function genericDebugCCC

    pure function genericDebugCCS( &
            module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = DEBUG_TYPE
    end function genericDebugCCS

    pure function genericDebugCSC( &
            module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = DEBUG_TYPE
    end function genericDebugCSC

    pure function genericDebugCSS( &
            module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = DEBUG_TYPE
    end function genericDebugCSS

    pure function genericDebugSCC( &
            module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = DEBUG_TYPE
    end function genericDebugSCC

    pure function genericDebugSCS( &
            module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = DEBUG_TYPE
    end function genericDebugSCS

    pure function genericDebugSSC( &
            module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = DEBUG_TYPE
    end function genericDebugSSC

    pure function genericDebugSSS( &
            module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = DEBUG_TYPE
    end function genericDebugSSS

    pure function debugWithTypeCCC( &
            type_tag, module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = type_tag
    end function debugWithTypeCCC

    pure function debugWithTypeCCS( &
            type_tag, module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = type_tag
    end function debugWithTypeCCS

    pure function debugWithTypeCSC( &
            type_tag, module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = type_tag
    end function debugWithTypeCSC

    pure function debugWithTypeCSS( &
            type_tag, module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = type_tag
    end function debugWithTypeCSS

    pure function debugWithTypeSCC( &
            type_tag, module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = type_tag
    end function debugWithTypeSCC

    pure function debugWithTypeSCS( &
            type_tag, module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = type_tag
    end function debugWithTypeSCS

    pure function debugWithTypeSSC( &
            type_tag, module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = type_tag
    end function debugWithTypeSSC

    pure function debugWithTypeSSS( &
            type_tag, module_name, procedure_name, level, message) result(debug_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(DebugLevel_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%call_stack = CallStack(module_name, procedure_name)
        debug_%level = level
        debug_%message = message
        debug_%message_type = type_tag
    end function debugWithTypeSSS

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

    pure function genericWarningCCC( &
            module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = WARNING_TYPE
    end function genericWarningCCC

    pure function genericWarningCCS( &
            module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = WARNING_TYPE
    end function genericWarningCCS

    pure function genericWarningCSC( &
            module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = WARNING_TYPE
    end function genericWarningCSC

    pure function genericWarningCSS( &
            module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = WARNING_TYPE
    end function genericWarningCSS

    pure function genericWarningSCC( &
            module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = WARNING_TYPE
    end function genericWarningSCC

    pure function genericWarningSCS( &
            module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = WARNING_TYPE
    end function genericWarningSCS

    pure function genericWarningSSC( &
            module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = WARNING_TYPE
    end function genericWarningSSC

    pure function genericWarningSSS( &
            module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = WARNING_TYPE
    end function genericWarningSSS

    pure function warningWithTypeCCC( &
            type_tag, module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = type_tag
    end function warningWithTypeCCC

    pure function warningWithTypeCCS( &
            type_tag, module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = type_tag
    end function warningWithTypeCCS

    pure function warningWithTypeCSC( &
            type_tag, module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = type_tag
    end function warningWithTypeCSC

    pure function warningWithTypeCSS( &
            type_tag, module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = type_tag
    end function warningWithTypeCSS

    pure function warningWithTypeSCC( &
            type_tag, module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = type_tag
    end function warningWithTypeSCC

    pure function warningWithTypeSCS( &
            type_tag, module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = type_tag
    end function warningWithTypeSCS

    pure function warningWithTypeSSC( &
            type_tag, module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = type_tag
    end function warningWithTypeSSC

    pure function warningWithTypeSSS( &
            type_tag, module_name, procedure_name, message) result(warning_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%call_stack = CallStack(module_name, procedure_name)
        warning_%message = message
        warning_%message_type = type_tag
    end function warningWithTypeSSS

    pure function genericFatalCCC( &
            module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = FATAL_TYPE
    end function genericFatalCCC

    pure function genericFatalCCS( &
            module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = FATAL_TYPE
    end function genericFatalCCS

    pure function genericFatalCSC( &
            module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = FATAL_TYPE
    end function genericFatalCSC

    pure function genericFatalCSS( &
            module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = FATAL_TYPE
    end function genericFatalCSS

    pure function genericFatalSCC( &
            module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = FATAL_TYPE
    end function genericFatalSCC

    pure function genericFatalSCS( &
            module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = FATAL_TYPE
    end function genericFatalSCS

    pure function genericFatalSSC( &
            module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = FATAL_TYPE
    end function genericFatalSSC

    pure function genericFatalSSS( &
            module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = FATAL_TYPE
    end function genericFatalSSS

    pure function fatalWithTypeCCC( &
            type_tag, module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = type_tag
    end function fatalWithTypeCCC

    pure function fatalWithTypeCCS( &
            type_tag, module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = type_tag
    end function fatalWithTypeCCS

    pure function fatalWithTypeCSC( &
            type_tag, module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = type_tag
    end function fatalWithTypeCSC

    pure function fatalWithTypeCSS( &
            type_tag, module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = type_tag
    end function fatalWithTypeCSS

    pure function fatalWithTypeSCC( &
            type_tag, module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = type_tag
    end function fatalWithTypeSCC

    pure function fatalWithTypeSCS( &
            type_tag, module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = type_tag
    end function fatalWithTypeSCS

    pure function fatalWithTypeSSC( &
            type_tag, module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = type_tag
    end function fatalWithTypeSSC

    pure function fatalWithTypeSSS( &
            type_tag, module_name, procedure_name, message) result(fatal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%call_stack = CallStack(module_name, procedure_name)
        fatal_%message = message
        fatal_%message_type = type_tag
    end function fatalWithTypeSSS

    pure function genericInternalCCC( &
            module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = INTERNAL_TYPE
    end function genericInternalCCC

    pure function genericInternalCCS( &
            module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = INTERNAL_TYPE
    end function genericInternalCCS

    pure function genericInternalCSC( &
            module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = INTERNAL_TYPE
    end function genericInternalCSC

    pure function genericInternalCSS( &
            module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = INTERNAL_TYPE
    end function genericInternalCSS

    pure function genericInternalSCC( &
            module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = INTERNAL_TYPE
    end function genericInternalSCC

    pure function genericInternalSCS( &
            module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = INTERNAL_TYPE
    end function genericInternalSCS

    pure function genericInternalSSC( &
            module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = INTERNAL_TYPE
    end function genericInternalSSC

    pure function genericInternalSSS( &
            module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = INTERNAL_TYPE
    end function genericInternalSSS

    pure function internalWithTypeCCC( &
            type_tag, module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = type_tag
    end function internalWithTypeCCC

    pure function internalWithTypeCCS( &
            type_tag, module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = type_tag
    end function internalWithTypeCCS

    pure function internalWithTypeCSC( &
            type_tag, module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = type_tag
    end function internalWithTypeCSC

    pure function internalWithTypeCSS( &
            type_tag, module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = type_tag
    end function internalWithTypeCSS

    pure function internalWithTypeSCC( &
            type_tag, module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = type_tag
    end function internalWithTypeSCC

    pure function internalWithTypeSCS( &
            type_tag, module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = type_tag
    end function internalWithTypeSCS

    pure function internalWithTypeSSC( &
            type_tag, module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = type_tag
    end function internalWithTypeSSC

    pure function internalWithTypeSSS( &
            type_tag, module_name, procedure_name, message) result(internal_)
        use Call_stack_m, only: CallStack
        use iso_varying_string, only: VARYING_STRING

        type(MessageType_t), intent(in) :: type_tag
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%call_stack = CallStack(module_name, procedure_name)
        internal_%message = message
        internal_%message_type = type_tag
    end function internalWithTypeSSS

    pure function prependNamesCC( &
            self, module_name, procedure_name) result(message)
        class(Message_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        class(Message_t), allocatable :: message

        allocate(message, source = self)
        message%call_stack = self%call_stack%prependNames( &
                module_name, procedure_name)
    end function prependNamesCC

    pure function prependNamesCS( &
            self, module_name, procedure_name) result(message)
        use iso_varying_string, only: VARYING_STRING

        class(Message_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        class(Message_t), allocatable :: message

        allocate(message, source = self)
        message%call_stack = self%call_stack%prependNames( &
                module_name, procedure_name)
    end function prependNamesCS

    pure function prependNamesSC( &
            self, module_name, procedure_name) result(message)
        use iso_varying_string, only: VARYING_STRING

        class(Message_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        class(Message_t), allocatable :: message

        allocate(message, source = self)
        message%call_stack = self%call_stack%prependNames( &
                module_name, procedure_name)
    end function prependNamesSC

    pure function prependNamesSS( &
            self, module_name, procedure_name) result(message)
        use iso_varying_string, only: VARYING_STRING

        class(Message_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        class(Message_t), allocatable :: message

        allocate(message, source = self)
        message%call_stack = self%call_stack%prependNames( &
                module_name, procedure_name)
    end function prependNamesSS

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

    pure function originatedFromProcedureC( &
            self, procedure_name) result(originated_from)
        class(Message_t), intent(in) :: self
        character(len=*), intent(in) :: procedure_name
        logical :: originated_from

        originated_from = self%call_stack.originatingProcedureIs.procedure_name
    end function originatedFromProcedureC

    pure function originatedFromProcedureS( &
            self, procedure_name) result(originated_from)
        use iso_varying_string, only: VARYING_STRING

        class(Message_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: procedure_name
        logical :: originated_from

        originated_from = self%call_stack.originatingProcedureIs.procedure_name
    end function originatedFromProcedureS

    pure function originatedFromModuleC( &
            self, module_name) result(originated_from)
        class(Message_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        logical :: originated_from

        originated_from = self%call_stack.originatingModuleIs.module_name
    end function originatedFromModuleC

    pure function originatedFromModuleS( &
            self, module_name) result(originated_from)
        use iso_varying_string, only: VARYING_STRING

        class(Message_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        logical :: originated_from

        originated_from = self%call_stack.originatingModuleIs.module_name
    end function originatedFromModuleS

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

    pure function debugTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(Debug_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = "DB-" // self%level%toString() // ": "
    end function debugTypeString

    pure function debugTypeRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(Debug_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = DEBUG_TYPE_STRING // "(level = " // self%level%toString() // ")"
    end function debugTypeRepr

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

    pure function warningTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Warning_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self); end associate

        string = "WN: "
    end function warningTypeString

    pure function warningTypeRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Warning_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self); end associate

        string = WARNING_TYPE_STRING
    end function warningTypeRepr

    pure function fatalTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Fatal_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self); end associate

        string = "FE: "
    end function fatalTypeString

    pure function fatalTypeRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Fatal_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self); end associate

        string = FATAL_TYPE_STRING
    end function fatalTypeRepr

    pure function internalTypeString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Internal_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self); end associate

        string = "IE: "
    end function internalTypeString

    pure function internalTypeRepr(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(Internal_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self); end associate

        string = INTERNAL_TYPE_STRING
    end function internalTypeRepr

    pure function debugLevelToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: toString

        class(DebugLevel_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = toString(self%level)
    end function debugLevelToString
end module Message_m
