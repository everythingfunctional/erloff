module Message_m
    use erloff_call_stack_m, only: call_stack_t
    use erloff_error_m, only: error_t, ERROR_TYPE_STRING
    use erloff_message_m, only: Message_t
    use erloff_message_type_m, only: message_type_t
    use iso_varying_string, only: &
            VARYING_STRING, assignment(=), operator(//), var_str
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: Procedure_t
    use strff, only: operator(.includes.), hanging_indent, to_string, NEWLINE

    implicit none
    private

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

    character(len=*), parameter :: FATAL_TYPE_STRING = "Fatal_t"
    character(len=*), parameter :: INTERNAL_TYPE_STRING = "Internal_t"

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

    public :: Fatal, Internal
contains
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
