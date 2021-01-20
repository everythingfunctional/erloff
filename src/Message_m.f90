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
end module Message_m
