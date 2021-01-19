module erloff_debug_m
    use erloff_call_stack_m, only: call_stack_t
    use erloff_debug_level_m, only: debug_level_t
    use erloff_message_m, only: message_t
    use erloff_message_type_m, only: message_type_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use iso_varying_string, only: varying_string, operator(//), var_str
    use strff, only: hanging_indent, NEWLINE

    implicit none
    private
    public :: debug_t, DEBUG

    type, extends(message_t) :: debug_t
        private
        type(debug_level_t) :: level
    contains
        private
        procedure, public :: with_names_prepended
        procedure, public :: type_string
        procedure, public :: repr
        procedure, public :: typeRepr => debugTypeRepr
        procedure, public :: is_type
    end type

    interface debug_t
        module procedure generic_debug_c
        module procedure generic_debug_s
        module procedure debug_with_type_c
        module procedure debug_with_type_s
    end interface

    character(len=*), parameter :: DEBUG_TYPE_STRING = "Debug_t"
    type(message_type_t), parameter :: DEBUG = message_type_t( &
            DEBUG_TYPE_STRING, .true.)
contains
    pure function generic_debug_c(module_, procedure_, level, message) result(debug_)
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(debug_level_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(debug_t) :: debug_

        debug_ = debug_t(DEBUG, module_, procedure_, level, var_str(message))
    end function

    pure function generic_debug_s(module_, procedure_, level, message) result(debug_)
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(debug_level_t), intent(in) :: level
        type(varying_string), intent(in) :: message
        type(debug_t) :: debug_

        debug_ = debug_t(DEBUG, module_, procedure_, level, message)
    end function

    pure function debug_with_type_c( &
            type_tag, module_, procedure_, level, message) result(debug_)
        type(message_type_t), intent(in) :: type_tag
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(debug_level_t), intent(in) :: level
        character(len=*), intent(in) :: message
        type(debug_t) :: debug_

        debug_ = debug_t(type_tag, module_, procedure_, level, var_str(message))
    end function

    pure function debug_with_type_s( &
            type_tag, module_, procedure_, level, message) result(debug_)
        type(message_type_t), intent(in) :: type_tag
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(debug_level_t), intent(in) :: level
        type(varying_string), intent(in) :: message
        type(debug_t) :: debug_

        debug_ = internal_constructor( &
                type_tag, call_stack_t(module_, procedure_), level, message)
    end function

    pure function internal_constructor( &
            type_tag, call_stack, level, message) result(debug_)
        type(message_type_t), intent(in) :: type_tag
        type(call_stack_t), intent(in) :: call_stack
        type(debug_level_t), intent(in) :: level
        type(varying_string), intent(in) :: message
        type(debug_t) :: debug_

        debug_%message_type = type_tag
        debug_%call_stack = call_stack
        debug_%level = level
        debug_%message = message
    end function

    function with_names_prepended(self, module_, procedure_) result(new_message)
        class(debug_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        class(message_t), allocatable :: new_message

        new_message = internal_constructor(&
                self%message_type, &
                self%call_stack%with_names_prepended(module_, procedure_), &
                self%level, &
                self%message)
    end function

    pure function type_string(self) result(string)
        class(debug_t), intent(in) :: self
        type(varying_string) :: string

        string = "DB-" // self%level%to_string() // ": "
    end function

    pure function repr(self)
        class(debug_t), intent(in) :: self
        type(varying_string) :: repr

        repr = hanging_indent( &
                'debug_t(' // NEWLINE &
                    // 'level = ' // self%level%repr() // ',' // NEWLINE &
                    // 'call_stack = ' // self%call_stack%repr() // ',' // NEWLINE &
                    // 'message_type = ' // self%message_type%repr() // ',' // NEWLINE &
                    // 'message = "' // self%message // '"', &
                4) // NEWLINE // ')'
    end function

    pure function debugTypeRepr(self) result(repr)
        class(debug_t), intent(in) :: self
        type(varying_string) :: repr

        repr = DEBUG_TYPE_STRING // '(level = ' // self%level%to_string() // ')'
    end function debugTypeRepr

    pure function is_type(self, type_tag)
        class(debug_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == DEBUG_TYPE_STRING) then
            is_type = .true.
        else
            is_type = self%message_type%description == type_tag%description
        end if
    end function
end module
