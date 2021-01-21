module erloff_message_m
    use erloff_call_stack_m, only: call_stack_t
    use erloff_message_type_m, only: message_type_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use iso_varying_string, only: varying_string, operator(//), var_str
    use strff, only: operator(.includes.), hanging_indent, NEWLINE

    implicit none
    private
    public :: message_t

    type, abstract :: message_t
        type(call_stack_t) :: call_stack
        type(varying_string) :: message
        type(message_type_t) :: message_type
    contains
        private
        procedure(prepend_names_i), public, deferred :: with_names_prepended
        procedure, public :: to_string => message_to_string
        procedure(to_string_i), public, deferred :: type_string
        procedure, public :: is_type
        generic, public :: operator(.isType.) => is_type
        procedure :: originated_from_module
        procedure :: originated_from_procedure
        generic, public :: operator(.originatedFrom.) => &
                originated_from_module, originated_from_procedure
        procedure :: is_from_module
        procedure :: is_from_procedure
        generic, public :: operator(.isFrom.) => &
                is_from_module, is_from_procedure
        procedure :: came_through_module
        procedure :: came_through_procedure
        generic, public :: operator(.cameThrough.) => &
                came_through_module, came_through_procedure
        procedure :: includes_c
        procedure :: includes_s
        generic, public :: operator(.includes.) => &
                includes_c, includes_s
        procedure :: includes_any_of
        generic, public :: operator(.includesAnyOf.) => includes_any_of
        procedure :: includes_all_of
        generic, public :: operator(.includesAllOf.) => includes_all_of
        procedure(to_string_i), public, deferred :: repr
    end type

    abstract interface
        pure function to_string_i(self) result(string)
            import :: message_t, varying_string

            implicit none

            class(message_t), intent(in) :: self
            type(varying_string) :: string
        end function

        function prepend_names_i(self, module_, procedure_) result(new_message)
            import :: message_t, module_t, procedure_t

            implicit none

            class(message_t), intent(in) :: self
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
            class(message_t), allocatable :: new_message
        end function
    end interface
contains
    pure subroutine prependNames(self, module_, procedure_)
        class(message_t), intent(inout) :: self
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_

        self%call_stack = self%call_stack%with_names_prepended(module_, procedure_)
    end subroutine prependNames

    pure function message_to_string(self) result(string)
        class(message_t), intent(in) :: self
        type(varying_string) :: string

        string = hanging_indent( &
                self%call_stack%to_string() // ":" // NEWLINE &
                    // self%type_string() // self%message_type%to_string() &
                    // self%message, &
                4)
    end function

    pure function is_type(self, type_tag)
        class(message_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        is_type = self%message_type%description == type_tag%description
    end function

    pure function originated_from_module(self, module_) result(originated_from)
        class(message_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: originated_from

        originated_from = self%call_stack.originatedFrom.module_
    end function

    pure function originated_from_procedure(self, procedure_) result(originated_from)
        class(message_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: originated_from

        originated_from = self%call_stack.originatedFrom.procedure_
    end function

    pure function is_from_module(self, module_) result(is_from)
        class(message_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: is_from

        is_from = self%call_stack.includes.module_
    end function

    pure function is_from_procedure(self, procedure_) result(is_from)
        class(message_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: is_from

        is_from = self%call_stack.includes.procedure_
    end function

    pure function came_through_module(self, module_) result(came_through)
        class(message_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: came_through

        came_through = &
                (.not.(self.originatedFrom.module_)) &
                .and.(self.isFrom.module_)
    end function

    pure function came_through_procedure(self, procedure_) result(came_through)
        class(message_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: came_through

        came_through = &
                (.not.(self.originatedFrom.procedure_)) &
                .and.(self.isFrom.procedure_)
    end function

    pure function includes_c(self, string) result(includes)
        class(message_t), intent(in) :: self
        character(len=*), intent(in) :: string
        logical :: includes

        includes = self.includes.var_str(string)
    end function

    pure function includes_s(self, string) result(includes)
        class(message_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        logical :: includes

        includes = self%message.includes.string
    end function

    pure function includes_any_of(self, strings) result(includes)
        class(message_t), intent(in) :: self
        type(varying_string), intent(in) :: strings(:)
        logical :: includes

        integer :: i
        logical :: includes_(size(strings))

        do i = 1, size(strings)
            includes_(i) = self.includes.strings(i)
        end do
        includes = any(includes_)
    end function

    pure function includes_all_of(self, strings) result(includes)
        class(message_t), intent(in) :: self
        type(varying_string), intent(in) :: strings(:)
        logical :: includes

        integer :: i
        logical :: includes_(size(strings))

        do i = 1, size(strings)
            includes_(i) = self.includes.strings(i)
        end do
        includes = all(includes_)
    end function
end module
