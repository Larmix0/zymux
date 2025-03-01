#include "lukip.h"

#include "resolver.c"

/** Test that the function for counting how many local captures a closure has counts correctly. */
PRIVATE_TEST_CASE(test_get_local_captures) {
    ClosedVariables closure = {.localCaptures = CREATE_DA()};
    APPEND_DA(&closure.localCaptures, 1);
    APPEND_DA(&closure.localCaptures, 22);
    ASSERT_INT_EQUAL(get_local_captures(&closure), 23);

    APPEND_DA(&closure.localCaptures, 0);
    APPEND_DA(&closure.localCaptures, 3);
    ASSERT_INT_EQUAL(get_local_captures(&closure), 26);

    FREE_DA(&closure.localCaptures);
}

/** Test that looking up variables retrieves the right ones when used. */
PRIVATE_TEST_CASE(test_find_variable_array_var) {
    VariableArray variables = CREATE_DA();
    Variable lookupVar = create_variable(
        false, true, false,
        create_token("10", TOKEN_INT_LIT), 2, create_var_resolution(5, VAR_GLOBAL), NULL
    );

    ASSERT_NULL(find_variable_array_var(&variables, lookupVar.name));
    APPEND_DA(
        &variables,
        create_variable(
            false, false, false,
            create_token("false", TOKEN_FALSE_KW), 0, create_var_resolution(0, VAR_LOCAL), NULL
        )
    );
    ASSERT_NULL(find_variable_array_var(&variables, lookupVar.name));

    APPEND_DA(&variables, lookupVar);
    ASSERT_TRUE(
        equal_token(find_variable_array_var(&variables, lookupVar.name)->name, lookupVar.name)
    );
    APPEND_DA(
        &variables,
        create_variable(
            false, true, false,
            create_token("nothing", TOKEN_IDENTIFIER), 2, create_var_resolution(0, VAR_GLOBAL), NULL
        )
    );
    ASSERT_TRUE(
        equal_token(find_variable_array_var(&variables, lookupVar.name)->name, lookupVar.name)
    );
    ASSERT_NULL(
        find_variable_array_var(&variables, create_token("nonexistent", TOKEN_IDENTIFIER))
    );

    FREE_DA(&variables);
}

/** Tests that popping the top scope of a variable array pops only the variables at top scope. */
PRIVATE_TEST_CASE(test_pop_top_scope) {
    VariableArray variables = CREATE_DA();
    APPEND_DA(
        &variables,
        create_variable(
            true, true, true,
            create_token("a", TOKEN_IDENTIFIER), 1, create_var_resolution(0, VAR_LOCAL), NULL
        )
    );
    APPEND_DA(
        &variables,
        create_variable(
            false, true, false,
            create_token("b", TOKEN_IDENTIFIER), 2, create_var_resolution(0, VAR_LOCAL), NULL
        )
    );
    APPEND_DA(
        &variables,
        create_variable(
            false, false, true,
            create_token("c", TOKEN_IDENTIFIER), 2, create_var_resolution(0, VAR_LOCAL), NULL
        )
    );
    APPEND_DA(
        &variables,
        create_variable(
            false, false, false,
            create_token("d", TOKEN_IDENTIFIER), 3, create_var_resolution(0, VAR_LOCAL), NULL
        )
    );
    ASSERT_INT_EQUAL(variables.length, 4);
    Resolver tempResolver = {.scopeDepth = 3};

    u32 popped = pop_top_scope(&tempResolver, &variables);
    ASSERT_INT_EQUAL(popped, 1);
    tempResolver.scopeDepth--;
    popped = pop_top_scope(&tempResolver, &variables);
    ASSERT_INT_EQUAL(popped, 2);
    tempResolver.scopeDepth--;
    popped = pop_top_scope(&tempResolver, &variables);
    ASSERT_INT_EQUAL(popped, 1);

    FREE_DA(&variables);
}

/** Tests resolver.c. */
void test_resolver() {
    TEST(test_get_local_captures);
    TEST(test_find_variable_array_var);
    TEST(test_pop_top_scope);
}
