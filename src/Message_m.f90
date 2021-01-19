module Message_m
    use erloff_call_stack_m, only: call_stack_t
    use erloff_debug_level_m, only: debug_level_t
    use erloff_message_m, only: Message_t
    use erloff_message_type_m, only: message_type_t
    use iso_varying_string, only: &
            VARYING_STRING, assignment(=), operator(//), var_str
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: Procedure_t
    use strff, only: operator(.includes.), hanging_indent, to_string, NEWLINE

    implicit none
    private

    type, public, extends(Message_t) :: Debug_t
        private
        type(debug_level_t) :: level
    contains
        private
        procedure, public :: type_string => debugTypeString
        procedure, public :: typeRepr => debugTypeRepr
        procedure, public :: is_type => debugIsType
    end type Debug_t

    type, public, extends(Message_t) :: Info_t
    contains
        private
        procedure, public :: type_string => infoTypeString
        procedure, public :: typeRepr => infoTypeRepr
        procedure, public :: is_type => infoIsType
    end type Info_t

    type, public, extends(Message_t) :: Warning_t
    contains
        private
        procedure, public :: type_string => warningTypeString
        procedure, public :: typeRepr => warningTypeRepr
        procedure, public :: is_type => warningIsType
    end type Warning_t

    type, public, abstract, extends(Message_t) :: Error_t
    contains
        private
        procedure, public :: isType => errorIsType
    end type Error_t

    type, public, extends(Error_t) :: Fatal_t
    contains
        private
        procedure, public :: type_string => fatalTypeString
        procedure, public :: typeRepr => fatalTypeRepr
        procedure, public :: is_type => fatalIsType
    end type Fatal_t

    type, public, extends(Error_t) :: Internal_t
    contains
        private
        procedure, public :: type_string => internalTypeString
        procedure, public :: typeRepr => internalTypeRepr
        procedure, public :: is_type => internalIsType
    end type Internal_t

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

    type(message_type_t), parameter, public :: DEBUG_TYPE = message_type_t( &
            DEBUG_TYPE_STRING, .true.)
    type(message_type_t), parameter, public :: INFO_TYPE = message_type_t( &
            INFO_TYPE_STRING, .true.)
    type(message_type_t), parameter, public :: WARNING_TYPE = message_type_t( &
            WARNING_TYPE_STRING, .true.)
    type(message_type_t), parameter, public :: ERROR_TYPE = message_type_t( &
            ERROR_TYPE_STRING, .true.)
    type(message_type_t), parameter, public :: FATAL_TYPE = message_type_t( &
            FATAL_TYPE_STRING, .true.)
    type(message_type_t), parameter, public :: INTERNAL_TYPE = message_type_t( &
            INTERNAL_TYPE_STRING, .true.)
    type(message_type_t), parameter, public :: INPUTS_TYPE = message_type_t( &
            "Inputs")
    type(message_type_t), parameter, public :: NOT_FOUND_TYPE = message_type_t( &
            "Not Found")
    type(message_type_t), parameter, public :: OUT_OF_BOUNDS_TYPE = message_type_t( &
            "Out of Bounds")
    type(message_type_t), parameter, public :: OUTPUTS_TYPE = message_type_t( &
            "Outputs")
    type(message_type_t), parameter, public :: OUTSIDE_NORMAL_RANGE_TYPE = &
            message_type_t("Outside Normal Range")
    type(message_type_t), parameter, public :: UNEQUAL_ARRAY_SIZES_TYPE = &
            message_type_t("Unequal Array Sizes")
    type(message_type_t), parameter, public :: UNKNOWN_TYPE_TYPE = &
            message_type_t("Unknown Type Encountered")

    public :: Debug, Info, Warning, Fatal, Internal
contains
    pure function genericDebugC(module_, procedure_, level, message) result(debug_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(debug_level_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_ = Debug(DEBUG_TYPE, module_, procedure_, level, var_str(message))
    end function genericDebugC

    pure function genericDebugS(module_, procedure_, level, message) result(debug_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(debug_level_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_ = Debug(DEBUG_TYPE, module_, procedure_, level, message)
    end function genericDebugS

    pure function debugWithTypeC( &
            type_tag, module_, procedure_, level, message) result(debug_)
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(debug_level_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(Debug_t) :: debug_

        debug_ = Debug(type_tag, module_, procedure_, level, var_str(message))
    end function debugWithTypeC

    pure function debugWithTypeS( &
            type_tag, module_, procedure_, level, message) result(debug_)
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(debug_level_t), intent(in) :: level
        type(VARYING_STRING), intent(in) :: message
        type(Debug_t) :: debug_

        debug_%message_type = type_tag
        debug_%call_stack = call_stack_t(module_, procedure_)
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
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Info_t) :: info_

        info_ = Info(type_tag, module_, procedure_, var_str(message))
    end function infoWithTypeC

    pure function infoWithTypeS(type_tag, module_, procedure_, message) result(info_)
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Info_t) :: info_

        info_%message_type = type_tag
        info_%call_stack = call_stack_t(module_, procedure_)
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
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Warning_t) :: warning_

        warning_ = Warning(type_tag, module_, procedure_, var_str(message))
    end function warningWithTypeC

    pure function warningWithTypeS(type_tag, module_, procedure_, message) result(warning_)
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Warning_t) :: warning_

        warning_%message_type = type_tag
        warning_%call_stack = call_stack_t(module_, procedure_)
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
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_ = Fatal(type_tag, module_, procedure_, var_str(message))
    end function fatalWithTypeC

    pure function fatalWithTypeS(type_tag, module_, procedure_, message) result(fatal_)
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Fatal_t) :: fatal_

        fatal_%message_type = type_tag
        fatal_%call_stack = call_stack_t(module_, procedure_)
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
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(Internal_t) :: internal_

        internal_ = Internal(type_tag, module_, procedure_, var_str(message))
    end function internalWithTypeC

    pure function internalWithTypeS(type_tag, module_, procedure_, message) result(internal_)
        type(message_type_t), intent(in) :: type_tag
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(VARYING_STRING), intent(in) :: message
        type(Internal_t) :: internal_

        internal_%message_type = type_tag
        internal_%call_stack = call_stack_t(module_, procedure_)
        internal_%message = message
    end function internalWithTypeS

    pure function debugTypeString(self) result(string)
        class(Debug_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = "DB-" // self%level%to_string() // ": "
    end function debugTypeString

    pure function debugTypeRepr(self) result(repr)
        class(Debug_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = DEBUG_TYPE_STRING // '(level = ' // self%level%to_string() // ')'
    end function debugTypeRepr

    pure function debugIsType(self, type_tag) result(is_type)
        class(Debug_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == DEBUG_TYPE_STRING) then
            is_type = .true.
        else
            is_type = self%message_type%description == type_tag%description
        end if
    end function

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

    pure function infoIsType(self, type_tag) result(is_type)
        class(Info_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == INFO_TYPE_STRING) then
            is_type = .true.
        else
            is_type = self%message_type%description == type_tag%description
        end if
    end function

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

    pure function warningIsType(self, type_tag) result(is_type)
        class(Warning_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == WARNING_TYPE_STRING) then
            is_type = .true.
        else
            is_type = self%message_type%description == type_tag%description
        end if
    end function

    pure function errorIsType(self, type_tag) result(is_type)
        class(Error_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == ERROR_TYPE_STRING) then
            is_type = .true.
        else
            is_type = self%message_type%description == type_tag%description
        end if
    end function

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

    pure function fatalIsType(self, type_tag) result(is_type)
        class(Fatal_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == ERROR_TYPE_STRING) then
            is_type = .true.
        else if (trim(type_tag%description) == FATAL_TYPE_STRING) then
            is_type = .true.
        else
            is_type = self%message_type%description == type_tag%description
        end if
    end function

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

    pure function internalIsType(self, type_tag) result(is_type)
        class(Internal_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == ERROR_TYPE_STRING) then
            is_type = .true.
        else if (trim(type_tag%description) == INTERNAL_TYPE_STRING) then
            is_type = .true.
        else
            is_type = self%message_type%description == type_tag%description
        end if
    end function
end module Message_m
