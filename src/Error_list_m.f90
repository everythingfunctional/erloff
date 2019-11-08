module Error_list_m
    use iso_varying_string, only: &
            VARYING_STRING, assignment(=), operator(//), var_str
    use Message_m, only: Error_t, MessageType_t
    use Module_m, only: Module_t
    use Procedure_m, only: Procedure_t
    use strff, only: hangingIndent, indent, join, NEWLINE

    implicit none
    private

    type :: ErrorItem_t
        class(Error_t), allocatable :: error
    end type ErrorItem_t

    type, public :: ErrorList_t
        private
        integer :: length = 0
        type(ErrorItem_t), allocatable :: errors(:)
    contains
        private
        procedure, public :: appendError
        procedure, public :: appendErrors
        procedure :: ofType
        generic, public :: operator(.ofType.) => ofType
        procedure :: ofTypes
        generic, public :: operator(.ofTypes.) => ofTypes
        procedure :: originatingFromModule
        procedure :: originatingFromModules
        procedure :: originatingFromProcedure
        procedure :: originatingFromProcedures
        generic, public :: operator(.originatingFrom.) => &
                originatingFromModule, &
                originatingFromModules, &
                originatingFromProcedure, &
                originatingFromProcedures
        procedure :: comingThroughModule
        procedure :: comingThroughModules
        procedure :: comingThroughProcedure
        procedure :: comingThroughProcedures
        generic, public :: operator(.comingThrough.) => &
                comingThroughModule, &
                comingThroughModules, &
                comingThroughProcedure, &
                comingThroughProcedures
        procedure :: fromModule
        procedure :: fromModules
        procedure :: fromProcedure
        procedure :: fromProcedures
        generic, public :: operator(.from.) => &
                fromModule, &
                fromModules, &
                fromProcedure, &
                fromProcedures
        procedure :: includingC
        procedure :: includingS
        generic, public :: operator(.including.) => includingC, includingS
        procedure :: includingAnyOf
        generic, public :: operator(.includingAnyOf.) => includingAnyOf
        procedure :: includingAllOf
        generic, public :: operator(.includingAllOf.) => includingAllOf
        procedure, public :: hasAny
        procedure :: hasType
        generic, public :: operator(.hasType.) => hasType
        procedure :: hasAnyOriginatingFromModule
        procedure :: hasAnyOriginatingFromProcedure
        generic, public :: operator(.hasAnyOriginatingFrom.) => &
                hasAnyOriginatingFromModule, hasAnyOriginatingFromProcedure
        procedure :: hasAnyComingThroughModule
        procedure :: hasAnyComingThroughProcedure
        generic, public :: operator(.hasAnyComingThrough.) => &
                hasAnyComingThroughModule, hasAnyComingThroughProcedure
        procedure :: hasAnyFromModule
        procedure :: hasAnyFromProcedure
        generic, public :: operator(.hasAnyFrom.) => &
                hasAnyFromModule, hasAnyFromProcedure
        procedure :: hasAnyIncludingC
        procedure :: hasAnyIncludingS
        generic, public :: operator(.hasAnyIncluding.) => &
                hasAnyIncludingC, hasAnyIncludingS
        procedure :: hasAnyIncludingAnyOf
        generic, public :: operator(.hasAnyIncludingAnyOf.) => &
                hasAnyIncludingAnyOf
        procedure :: hasAnyIncludingAllOf
        generic, public :: operator(.hasAnyIncludingAllOf.) => &
                hasAnyIncludingAllOf
        procedure, public :: toString
        procedure, public :: repr
    end type ErrorList_t

    interface size
        module procedure errorListSize
    end interface size

    public :: size
contains
    pure subroutine appendError(self, error)
        class(ErrorList_t), intent(inout) :: self
        class(Error_t), intent(in) :: error

        type(ErrorItem_t) :: old_errors(self%length)

        if (self%length == 0) then
            allocate(self%errors(1))
            allocate(self%errors(1)%error, source = error)
            self%length = 1
        else
            self%length = self%length + 1
            old_errors = self%errors
            deallocate(self%errors)
            allocate(self%errors(self%length))
            self%errors(1:size(old_errors)) = old_errors
            allocate(self%errors(self%length)%error, source = error)
        end if
    end subroutine appendError

    pure subroutine appendErrors(self, errors, module_, procedure_)
        class(ErrorList_t), intent(inout) :: self
        type(ErrorList_t), intent(in) :: errors
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_

        integer :: i
        type(ErrorItem_t) :: old_errors(self%length)
        integer :: num_new_errors
        integer :: num_old_errors
        integer :: total_num_errors

        if (.not. errors%length == 0) then
            if (self%length == 0) then
                allocate(self%errors, source = errors%errors)
                self%length = size(self%errors)
                do i = 1, self%length
                    call self%errors(i)%error%prependNames(module_, procedure_)
                end do
            else
                num_old_errors = self%length
                num_new_errors = errors%length
                total_num_errors = num_old_errors + num_new_errors
                old_errors = self%errors
                deallocate(self%errors)
                allocate(self%errors(total_num_errors))
                self%errors(1:num_old_errors) = old_errors
                self%errors(num_old_errors+1:) = errors%errors
                do i = num_old_errors + 1, total_num_errors
                    call self%errors(i)%error%prependNames(module_, procedure_)
                end do
                self%length = total_num_errors
            end if
        end if
    end subroutine appendErrors

    pure function ofType(self, type_tag) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tag
        type(ErrorList_t) :: new_list

        new_list = self.ofTypes.[type_tag]
    end function ofType

    pure function ofTypes(self, type_tags) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tags(:)
        type(ErrorList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(type_tags))
        integer :: num_output
        integer :: num_tags

        if (.not. self%length == 0) then
            num_tags = size(type_tags)
            do i = 1, num_tags
                do j = 1, self%length
                    individual_masks(j, i) = self%errors(j)%error.isType.type_tags(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%errors(num_output))
                new_list%length = num_output
                new_list%errors = pack(self%errors, mask=final_mask)
            end if
        end if
    end function ofTypes

    pure function originatingFromModule(self, module_) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        type(ErrorList_t) :: new_list

        new_list = self.originatingFrom.[module_]
    end function originatingFromModule

    pure function originatingFromModules(self, modules) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Module_t), intent(in) :: modules(:)
        type(ErrorList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(modules))
        integer :: num_modules
        integer :: num_output

        if (.not. self%length == 0) then
            num_modules = size(modules)
            do i = 1, num_modules
                do j = 1, self%length
                    individual_masks(j, i) = self%errors(j)%error.originatedFrom.modules(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%errors(num_output))
                new_list%length = num_output
                new_list%errors = pack(self%errors, mask=final_mask)
            end if
        end if
    end function originatingFromModules

    pure function originatingFromProcedure(self, procedure_) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        type(ErrorList_t) :: new_list

        new_list = self.originatingFrom.[procedure_]
    end function originatingFromProcedure

    pure function originatingFromProcedures(self, procedures) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedures(:)
        type(ErrorList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(procedures))
        integer :: num_output
        integer :: num_procedures

        if (.not. self%length == 0) then
            num_procedures = size(procedures)
            do i = 1, num_procedures
                do j = 1, self%length
                    individual_masks(j, i) = self%errors(j)%error.originatedFrom.procedures(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%errors(num_output))
                new_list%length = num_output
                new_list%errors = pack(self%errors, mask=final_mask)
            end if
        end if
    end function originatingFromProcedures

    pure function comingThroughModule(self, module_) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        type(ErrorList_t) :: new_list

        new_list = self.comingThrough.[module_]
    end function comingThroughModule

    pure function comingThroughModules(self, modules) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Module_t), intent(in) :: modules(:)
        type(ErrorList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(modules))
        integer :: num_modules
        integer :: num_output

        if (.not. self%length == 0) then
            num_modules = size(modules)
            do i = 1, num_modules
                do j = 1, self%length
                    individual_masks(j, i) = self%errors(j)%error.cameThrough.modules(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%errors(num_output))
                new_list%length = num_output
                new_list%errors = pack(self%errors, mask=final_mask)
            end if
        end if
    end function comingThroughModules

    pure function comingThroughProcedure(self, procedure_) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        type(ErrorList_t) :: new_list

        new_list = self.comingThrough.[procedure_]
    end function comingThroughProcedure

    pure function comingThroughProcedures(self, procedures) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedures(:)
        type(ErrorList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(procedures))
        integer :: num_output
        integer :: num_procedures

        if (.not. self%length == 0) then
            num_procedures = size(procedures)
            do i = 1, num_procedures
                do j = 1, self%length
                    individual_masks(j, i) = self%errors(j)%error.cameThrough.procedures(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%errors(num_output))
                new_list%length = num_output
                new_list%errors = pack(self%errors, mask=final_mask)
            end if
        end if
    end function comingThroughProcedures

    pure function fromModule(self, module_) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        type(ErrorList_t) :: new_list

        new_list = self.from.[module_]
    end function fromModule

    pure function fromModules(self, modules) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Module_t), intent(in) :: modules(:)
        type(ErrorList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(modules))
        integer :: num_modules
        integer :: num_output

        if (.not. self%length == 0) then
            num_modules = size(modules)
            do i = 1, num_modules
                do j = 1, self%length
                    individual_masks(j, i) = self%errors(j)%error.isFrom.modules(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%errors(num_output))
                new_list%length = num_output
                new_list%errors = pack(self%errors, mask=final_mask)
            end if
        end if
    end function fromModules

    pure function fromProcedure(self, procedure_) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        type(ErrorList_t) :: new_list

        new_list = self.from.[procedure_]
    end function fromProcedure

    pure function fromProcedures(self, procedures) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedures(:)
        type(ErrorList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(procedures))
        integer :: num_output
        integer :: num_procedures

        if (.not. self%length == 0) then
            num_procedures = size(procedures)
            do i = 1, num_procedures
                do j = 1, self%length
                    individual_masks(j, i) = self%errors(j)%error.isFrom.procedures(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%errors(num_output))
                new_list%length = num_output
                new_list%errors = pack(self%errors, mask=final_mask)
            end if
        end if
    end function fromProcedures

    pure function includingC(self, string) result(new_list)
        class(ErrorList_t), intent(in) :: self
        character(len=*), intent(in) :: string
        type(ErrorList_t) :: new_list

        new_list = self.including.var_str(string)
    end function includingC

    pure function includingS(self, string) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t) :: new_list

        new_list = self.includingAnyOf.[string]
    end function includingS

    pure function includingAnyOf(self, strings) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        type(ErrorList_t) :: new_list

        integer :: i
        logical :: mask(self%length)
        integer :: num_output

        if (.not. self%length == 0) then
            do i = 1, self%length
                mask(i) = self%errors(i)%error.includesAnyOf.strings
            end do
            num_output = count(mask)
            if (num_output > 0) then
                allocate(new_list%errors(num_output))
                new_list%errors = pack(self%errors, mask = mask)
                new_list%length = num_output
            end if
        end if
    end function includingAnyOf

    pure function includingAllOf(self, strings) result(new_list)
        class(ErrorList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        type(ErrorList_t) :: new_list

        integer :: i
        logical :: mask(self%length)
        integer :: num_output

        if (.not. self%length == 0) then
            do i = 1, self%length
                mask(i) = self%errors(i)%error.includesAllOf.strings
            end do
            num_output = count(mask)
            if (num_output > 0) then
                allocate(new_list%errors(num_output))
                new_list%errors = pack(self%errors, mask = mask)
                new_list%length = num_output
            end if
        end if
    end function includingAllOf

    pure function hasAny(self)
        class(ErrorList_t), intent(in) :: self
        logical :: hasAny

        hasAny = size(self) > 0
    end function hasAny

    pure function hasType(self, type_tag)
        class(ErrorList_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tag
        logical :: hasType

        hasType = size(self.ofType.type_tag) > 0
    end function hasType

    pure function hasAnyOriginatingFromModule(self, module_) result(has_any)
        class(ErrorList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: has_any

        has_any = size(self.originatingFrom.module_) > 0
    end function hasAnyOriginatingFromModule

    pure function hasAnyOriginatingFromProcedure(self, procedure_) result(has_any)
        class(ErrorList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: has_any

        has_any = size(self.originatingFrom.procedure_) > 0
    end function hasAnyOriginatingFromProcedure

    pure function hasAnyComingThroughModule(self, module_) result(has_any)
        class(ErrorList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: has_any

        has_any = size(self.comingThrough.module_) > 0
    end function hasAnyComingThroughModule

    pure function hasAnyComingThroughProcedure(self, procedure_) result(has_any)
        class(ErrorList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: has_any

        has_any = size(self.comingThrough.procedure_) > 0
    end function hasAnyComingThroughProcedure

    pure function hasAnyFromModule(self, module_) result(has_any)
        class(ErrorList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: has_any

        has_any = size(self.from.module_) > 0
    end function hasAnyFromModule

    pure function hasAnyFromProcedure(self, procedure_) result(has_any)
        class(ErrorList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: has_any

        has_any = size(self.from.procedure_) > 0
    end function hasAnyFromProcedure

    pure function hasAnyIncludingC(self, string) result(has_any)
        class(ErrorList_t), intent(in) :: self
        character(len=*), intent(in) :: string
        logical :: has_any

        has_any = size(self.including.string) > 0
    end function hasAnyIncludingC

    pure function hasAnyIncludingS(self, string) result(has_any)
        class(ErrorList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        logical :: has_any

        has_any = size(self.including.string) > 0
    end function hasAnyIncludingS

    pure function hasAnyIncludingAnyOf(self, strings) result(has_any)
        class(ErrorList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        logical :: has_any

        has_any = size(self.includingAnyOf.strings) > 0
    end function hasAnyIncludingAnyOf

    pure function hasAnyIncludingAllOf(self, strings) result(has_any)
        class(ErrorList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        logical :: has_any

        has_any = size(self.includingAllOf.strings) > 0
    end function hasAnyIncludingAllOf

    pure function toString(self) result(string)
        class(ErrorList_t), intent(in) :: self
        type(VARYING_STRING) :: string

        integer :: i
        type(VARYING_STRING) :: strings(self%length)

        if (self%length == 0) then
            string = ""
        else
            do i = 1, self%length
                strings(i) = self%errors(i)%error%toString()
            end do
            string = join(strings, NEWLINE)
        end if
    end function toString

    pure function repr(self)
        class(ErrorList_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        integer :: i
        type(VARYING_STRING) :: strings(self%length)

        do i = 1, self%length
            strings(i) = self%errors(i)%error%repr()
        end do

        repr = hangingIndent( &
                "MessageList_t(" // NEWLINE &
                    // "messages = [" // NEWLINE &
                    // indent(join(strings, "," // NEWLINE), 4) // NEWLINE // "]", &
                4) // NEWLINE // ")"
    end function repr

    pure function errorListSize(self) result(length)
        class(ErrorList_t), intent(in) :: self
        integer :: length

        length = self%length
    end function errorListSize
end module Error_list_m
