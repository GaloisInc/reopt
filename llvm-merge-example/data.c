/*
 * This file contains global data declarations.
 *
 * It is used so that the code does not contain separate copies of global data.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "data.h"

const char *w_str = "w";
const char *r_str = "r";
const char sep = ',';

struct tree g_left = { .left = NULL, .right = NULL, .payload = "left" };
struct tree g_right_right = { .left = NULL, .right = NULL, .payload = "right_right" };
struct tree g_right = { .left = NULL, .right = &g_right_right, .payload = "right" };
struct tree root = { .left = &g_left, .right = &g_right, .payload = "root" };
