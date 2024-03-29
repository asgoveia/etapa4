/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 2 "parser.y" /* yacc.c:339  */

    #include <stdio.h>
    #include <stdlib.h>
    #include "lex.yy.h"
    #include "hash.h"
    #include "astree.h"
    #include "semantic.h"
    #include "tac.h"

    int getLineNumber();
    int yyerror(char *message);
    
    AST *root;
  

#line 82 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    KW_BYTE = 258,
    KW_INT = 259,
    KW_LONG = 260,
    KW_FLOAT = 261,
    KW_BOOL = 262,
    KW_IF = 263,
    KW_THEN = 264,
    KW_ELSE = 265,
    KW_WHILE = 266,
    KW_FOR = 267,
    KW_READ = 268,
    KW_PRINT = 269,
    KW_RETURN = 270,
    KW_BREAK = 271,
    OPERATOR_LE = 272,
    OPERATOR_GE = 273,
    OPERATOR_EQ = 274,
    OPERATOR_DIF = 275,
    TK_IDENTIFIER = 276,
    LIT_INTEGER = 277,
    LIT_FLOAT = 278,
    LIT_TRUE = 279,
    LIT_FALSE = 280,
    LIT_CHAR = 281,
    LIT_STRING = 282
  };
#endif
/* Tokens.  */
#define KW_BYTE 258
#define KW_INT 259
#define KW_LONG 260
#define KW_FLOAT 261
#define KW_BOOL 262
#define KW_IF 263
#define KW_THEN 264
#define KW_ELSE 265
#define KW_WHILE 266
#define KW_FOR 267
#define KW_READ 268
#define KW_PRINT 269
#define KW_RETURN 270
#define KW_BREAK 271
#define OPERATOR_LE 272
#define OPERATOR_GE 273
#define OPERATOR_EQ 274
#define OPERATOR_DIF 275
#define TK_IDENTIFIER 276
#define LIT_INTEGER 277
#define LIT_FLOAT 278
#define LIT_TRUE 279
#define LIT_FALSE 280
#define LIT_CHAR 281
#define LIT_STRING 282

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 19 "parser.y" /* yacc.c:355  */

        HASH_NODE *symbol;
        AST *ast;

#line 181 "y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 198 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  10
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   342

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  47
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  20
/* YYNRULES -- Number of rules.  */
#define YYNRULES  74
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  143

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   282

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      37,    38,    35,    33,    44,    34,    30,    36,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    43,    40,
      31,    39,    32,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    41,     2,    42,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    29,     2,
       2,     2,     2,    45,     2,    46,    28,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    87,    87,    95,    96,   100,   101,   102,   106,   107,
     108,   109,   110,   114,   115,   116,   117,   118,   122,   123,
     124,   129,   130,   134,   135,   139,   143,   144,   148,   149,
     154,   158,   159,   160,   164,   165,   166,   167,   168,   169,
     170,   171,   172,   173,   174,   175,   179,   183,   184,   188,
     189,   193,   194,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "KW_BYTE", "KW_INT", "KW_LONG",
  "KW_FLOAT", "KW_BOOL", "KW_IF", "KW_THEN", "KW_ELSE", "KW_WHILE",
  "KW_FOR", "KW_READ", "KW_PRINT", "KW_RETURN", "KW_BREAK", "OPERATOR_LE",
  "OPERATOR_GE", "OPERATOR_EQ", "OPERATOR_DIF", "TK_IDENTIFIER",
  "LIT_INTEGER", "LIT_FLOAT", "LIT_TRUE", "LIT_FALSE", "LIT_CHAR",
  "LIT_STRING", "'~'", "'v'", "'.'", "'<'", "'>'", "'+'", "'-'", "'*'",
  "'/'", "'('", "')'", "'='", "';'", "'['", "']'", "':'", "','", "'{'",
  "'}'", "$accept", "program", "ldecl", "decl", "type", "scalar",
  "scalarNoBool", "arrayInit", "listLit", "param", "parameterList", "rest",
  "block", "printList", "cmd", "lcmd", "cmdrest", "expParam",
  "expParamRest", "exp", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   126,   118,
      46,    60,    62,    43,    45,    42,    47,    40,    41,    61,
      59,    91,    93,    58,    44,   123,   125
};
# endif

#define YYPACT_NINF -70

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-70)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      26,   -70,   -70,   -70,   -70,   -70,     1,   -70,    26,     4,
     -70,   -70,   -20,    26,    33,    13,    20,    16,    29,   -70,
     -70,   -70,   -70,   -70,    28,    40,   -70,    26,   -70,    41,
     -70,    42,    16,    -6,   -70,    47,    31,   -70,    51,    57,
      58,    80,   288,   305,   -70,   -15,   -70,    74,    69,   -70,
     -70,   -70,    47,   -70,   305,   305,    95,   -70,   -21,   -70,
     -70,   -70,   -70,   -70,   288,   305,   305,   -70,   239,   260,
     305,   305,    -6,   -70,   -70,    47,   -70,    45,   173,    75,
     305,   305,   -70,   260,   195,   305,   305,   305,   305,   305,
     305,   305,   305,   305,   305,   305,   305,   -70,   260,   133,
      74,   -70,   110,    -6,   305,    82,    73,   153,   -70,    64,
      64,    64,    64,   267,   287,    64,    64,   -22,   -22,   -70,
     -70,    96,   -70,    -6,   -70,    93,   -70,   305,   -70,   -70,
     305,   111,   305,    73,   260,    -6,   113,   -70,   -70,   305,
     217,    -6,   -70
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       4,     9,    10,    11,    12,     8,     0,     2,     4,     0,
       1,     3,     0,    27,     0,     0,     0,    29,     0,    14,
      13,    16,    17,    15,     0,     0,    25,     0,    26,     0,
       6,    22,    29,    45,     5,     0,     0,    28,     0,     0,
       0,     0,    33,     0,    43,     0,    44,    48,     0,    18,
      19,    20,    24,     7,     0,     0,     0,    36,    53,    57,
      56,    59,    60,    58,    33,     0,     0,    37,    33,    38,
       0,     0,    45,    46,    30,    24,    21,     0,     0,     0,
      49,     0,    31,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    32,    34,     0,
      48,    23,     0,    45,     0,     0,    52,     0,    61,    71,
      72,    73,    74,    68,    69,    67,    66,    62,    63,    64,
      65,     0,    47,    45,    41,     0,    55,     0,    50,    54,
       0,    40,     0,    52,    35,    45,     0,    51,    39,     0,
       0,    45,    42
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -70,   -70,   126,   -70,    -9,   -70,   101,   -70,    63,   112,
     -70,   108,   125,   -28,   -69,   -70,    55,   -70,     8,   -43
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     6,     7,     8,     9,    24,    75,    36,    76,    17,
      18,    28,    46,    67,    47,    48,    73,   105,   128,    68
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      69,    10,    38,   100,    16,    39,    40,    41,    42,    43,
      44,    77,    78,    95,    96,    45,    80,    13,    16,    14,
      81,    15,    83,    84,    70,    12,    71,    98,    99,     1,
       2,     3,     4,     5,   124,    25,    82,   106,   107,    33,
      97,    26,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   131,    19,    20,    21,    22,    23,
      27,   125,    85,    86,    87,    88,   138,    29,    30,    49,
      50,    53,   142,    51,    89,    90,    91,    92,    93,    94,
      95,    96,    31,   102,   133,    35,    33,   134,    54,   136,
      85,    86,    87,    88,    55,    56,   140,    93,    94,    95,
      96,    57,    89,    90,    91,    92,    93,    94,    95,    96,
      85,    86,    87,    88,    72,    74,    79,   127,   104,   123,
     126,   135,    89,    90,    91,    92,    93,    94,    95,    96,
      85,    86,    87,    88,    11,   130,    52,   132,   101,    32,
      37,   137,    89,    90,    91,    92,    93,    94,    95,    96,
      85,    86,    87,    88,    34,   122,     0,   139,     0,     0,
       0,     0,    89,    90,    91,    92,    93,    94,    95,    96,
      85,    86,    87,    88,     0,   121,     0,     0,     0,     0,
       0,     0,    89,    90,    91,    92,    93,    94,    95,    96,
      85,    86,    87,    88,     0,   129,     0,     0,     0,     0,
       0,     0,    89,    90,    91,    92,    93,    94,    95,    96,
       0,   103,    85,    86,    87,    88,     0,     0,     0,     0,
       0,     0,     0,     0,    89,    90,    91,    92,    93,    94,
      95,    96,     0,   108,    85,    86,    87,    88,     0,     0,
       0,     0,     0,     0,     0,     0,    89,    90,    91,    92,
      93,    94,    95,    96,     0,   141,    85,    86,    87,    88,
      58,    59,    60,    61,    62,    63,    64,    65,    89,    90,
      91,    92,    93,    94,    95,    96,    66,    85,    86,    87,
      88,     0,     0,     0,    85,    86,    87,    88,     0,    89,
      90,    91,    92,    93,    94,    95,    96,    90,    91,    92,
      93,    94,    95,    96,    85,    86,    87,    88,     0,    58,
      59,    60,    61,    62,    63,    64,    65,     0,    91,    92,
      93,    94,    95,    96,     0,    66,    58,    59,    60,    61,
      62,    63,     0,    65,     0,     0,     0,     0,     0,     0,
       0,     0,    66
};

static const yytype_int16 yycheck[] =
{
      43,     0,     8,    72,    13,    11,    12,    13,    14,    15,
      16,    54,    55,    35,    36,    21,    37,    37,    27,    39,
      41,    41,    65,    66,    39,    21,    41,    70,    71,     3,
       4,     5,     6,     7,   103,    22,    64,    80,    81,    45,
      68,    21,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,   123,    22,    23,    24,    25,    26,
      44,   104,    17,    18,    19,    20,   135,    38,    40,    22,
      23,    40,   141,    26,    29,    30,    31,    32,    33,    34,
      35,    36,    42,    38,   127,    43,    45,   130,    37,   132,
      17,    18,    19,    20,    37,    37,   139,    33,    34,    35,
      36,    21,    29,    30,    31,    32,    33,    34,    35,    36,
      17,    18,    19,    20,    40,    46,    21,    44,    43,     9,
      38,    10,    29,    30,    31,    32,    33,    34,    35,    36,
      17,    18,    19,    20,     8,    39,    35,    44,    75,    27,
      32,   133,    29,    30,    31,    32,    33,    34,    35,    36,
      17,    18,    19,    20,    29,   100,    -1,    44,    -1,    -1,
      -1,    -1,    29,    30,    31,    32,    33,    34,    35,    36,
      17,    18,    19,    20,    -1,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    29,    30,    31,    32,    33,    34,    35,    36,
      17,    18,    19,    20,    -1,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    29,    30,    31,    32,    33,    34,    35,    36,
      -1,    38,    17,    18,    19,    20,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    29,    30,    31,    32,    33,    34,
      35,    36,    -1,    38,    17,    18,    19,    20,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    29,    30,    31,    32,
      33,    34,    35,    36,    -1,    38,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    17,    18,    19,
      20,    -1,    -1,    -1,    17,    18,    19,    20,    -1,    29,
      30,    31,    32,    33,    34,    35,    36,    30,    31,    32,
      33,    34,    35,    36,    17,    18,    19,    20,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    -1,    31,    32,
      33,    34,    35,    36,    -1,    37,    21,    22,    23,    24,
      25,    26,    -1,    28,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     7,    48,    49,    50,    51,
       0,    49,    21,    37,    39,    41,    51,    56,    57,    22,
      23,    24,    25,    26,    52,    22,    21,    44,    58,    38,
      40,    42,    56,    45,    59,    43,    54,    58,     8,    11,
      12,    13,    14,    15,    16,    21,    59,    61,    62,    22,
      23,    26,    53,    40,    37,    37,    37,    21,    21,    22,
      23,    24,    25,    26,    27,    28,    37,    60,    66,    66,
      39,    41,    40,    63,    46,    53,    55,    66,    66,    21,
      37,    41,    60,    66,    66,    17,    18,    19,    20,    29,
      30,    31,    32,    33,    34,    35,    36,    60,    66,    66,
      61,    55,    38,    38,    43,    64,    66,    66,    38,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    42,    63,     9,    61,    66,    38,    44,    65,    42,
      39,    61,    44,    66,    66,    10,    66,    65,    61,    44,
      66,    38,    61
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    47,    48,    49,    49,    50,    50,    50,    51,    51,
      51,    51,    51,    52,    52,    52,    52,    52,    53,    53,
      53,    54,    54,    55,    55,    56,    57,    57,    58,    58,
      59,    60,    60,    60,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    62,    63,    63,    64,
      64,    65,    65,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     0,     6,     5,     7,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     0,     2,     0,     2,     2,     0,     3,     0,
       3,     2,     2,     0,     3,     6,     2,     2,     2,     8,
       6,     5,    11,     1,     1,     0,     2,     3,     0,     0,
       2,     3,     0,     1,     4,     4,     1,     1,     1,     1,
       1,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       2,     3,     3,     3,     3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 87 "parser.y" /* yacc.c:1646  */
    {root = (yyvsp[0].ast); 
                                                                astreePrint((yyvsp[0].ast), 0);
                                                                checkSemantics((yyvsp[0].ast));
                                                                fprintf(stderr, "%d semantic errors.\n", getSemanticErrors());
                                                                tacPrintBackwards(generateCode((yyvsp[0].ast)));}
#line 1416 "y.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 95 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_LDECL, 0, (yyvsp[-1].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1422 "y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 96 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=0;}
#line 1428 "y.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 100 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_FUNC, (yyvsp[-4].symbol), (yyvsp[-5].ast), (yyvsp[-2].ast), (yyvsp[0].ast), 0, getLineNumber());}
#line 1434 "y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 101 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_VARDEC, (yyvsp[-3].symbol), (yyvsp[-4].ast), (yyvsp[-1].ast), 0, 0, getLineNumber());}
#line 1440 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 102 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_VEC, (yyvsp[-5].symbol), (yyvsp[-6].ast), astreeCreate(AST_SYMBOL, (yyvsp[-3].symbol), 0, 0, 0, 0, getLineNumber()), (yyvsp[-1].ast), 0, getLineNumber());}
#line 1446 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 106 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_BOOL, 0, 0, 0, 0, 0, getLineNumber());}
#line 1452 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 107 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_BYTE, 0, 0, 0, 0, 0, getLineNumber());}
#line 1458 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 108 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_INT, 0, 0, 0, 0, 0, getLineNumber());}
#line 1464 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 109 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_LONG, 0, 0, 0, 0, 0, getLineNumber());}
#line 1470 "y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 110 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_FLOAT, 0, 0, 0, 0, 0, getLineNumber());}
#line 1476 "y.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 114 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1482 "y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 115 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1488 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 116 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1494 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 117 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1500 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 118 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1506 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 122 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1512 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 123 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1518 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 124 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1524 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 129 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_FIRST_LLIT, 0, (yyvsp[-1].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1530 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 130 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=0;}
#line 1536 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 134 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_LLIT, 0, (yyvsp[-1].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1542 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 135 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=0;}
#line 1548 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 139 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_PARAM, (yyvsp[0].symbol), (yyvsp[-1].ast), 0, 0, 0, getLineNumber());}
#line 1554 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 143 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_LPARAM,0,(yyvsp[-1].ast),(yyvsp[0].ast),0,0, getLineNumber());}
#line 1560 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 144 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=0;}
#line 1566 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 148 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_RPARAM, 0, (yyvsp[-1].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1572 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 149 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=0;}
#line 1578 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 154 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_BLOCK, 0, (yyvsp[-1].ast), 0, 0, 0, getLineNumber());}
#line 1584 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 158 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_LPRINT, (yyvsp[-1].symbol), (yyvsp[0].ast), 0, 0, 0, getLineNumber()); }
#line 1590 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 159 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_EPRINT, 0, (yyvsp[-1].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1596 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 160 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=0;}
#line 1602 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 164 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_ASS, (yyvsp[-2].symbol), (yyvsp[0].ast), 0, 0, 0, getLineNumber()); }
#line 1608 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 165 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_VECEXP, (yyvsp[-5].symbol), (yyvsp[-3].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1614 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 166 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_READ, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber()); }
#line 1620 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 167 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_PRINT, 0, (yyvsp[0].ast), 0, 0, 0, getLineNumber());}
#line 1626 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 168 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_RET, 0, (yyvsp[0].ast), 0, 0, 0, getLineNumber());}
#line 1632 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 169 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_IFELSE, 0, (yyvsp[-5].ast), (yyvsp[-2].ast), (yyvsp[0].ast), 0, getLineNumber());}
#line 1638 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 170 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_IF, 0, (yyvsp[-3].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1644 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 171 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_WHILE, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1650 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 172 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_FOR, (yyvsp[-8].symbol), (yyvsp[-6].ast), (yyvsp[-4].ast), (yyvsp[-2].ast), (yyvsp[0].ast), getLineNumber());}
#line 1656 "y.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 173 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_BREAK, 0, 0, 0, 0, 0, getLineNumber());}
#line 1662 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 174 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=(yyvsp[0].ast);}
#line 1668 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 175 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=0;}
#line 1674 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 179 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_LCMD, 0, (yyvsp[-1].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1680 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 183 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_RCMD, 0, (yyvsp[-1].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1686 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 184 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=0;}
#line 1692 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 188 "parser.y" /* yacc.c:1646  */
    { (yyval.ast)=0; }
#line 1698 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 189 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_EPARAM, 0, (yyvsp[-1].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1704 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 193 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_EPARAMREST, 0, (yyvsp[-1].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1710 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 194 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=0;}
#line 1716 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 198 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1722 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 199 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_VECREAD, (yyvsp[-3].symbol), (yyvsp[-1].ast), 0, 0, 0, getLineNumber());}
#line 1728 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 200 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_IDEXP, (yyvsp[-3].symbol), (yyvsp[-1].ast), 0, 0, 0, getLineNumber());}
#line 1734 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 201 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1740 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 202 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1746 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 203 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1752 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 204 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1758 "y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 205 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SYMBOL, (yyvsp[0].symbol), 0, 0, 0, 0, getLineNumber());}
#line 1764 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 206 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_EXP, 0, (yyvsp[-1].ast), 0, 0, 0, getLineNumber());}
#line 1770 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 207 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_ADD, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1776 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 208 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_SUB, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1782 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 209 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_MUL, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1788 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 210 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_DIV, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1794 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 211 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_GREATER, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1800 "y.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 212 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_LESSER, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1806 "y.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 213 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_OR, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1812 "y.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 214 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_AND, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1818 "y.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 215 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_NOT, 0, (yyvsp[0].ast), 0, 0, 0, getLineNumber());}
#line 1824 "y.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 216 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_LE, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1830 "y.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 217 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_GE, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1836 "y.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 218 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_EQUAL, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1842 "y.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 219 "parser.y" /* yacc.c:1646  */
    {(yyval.ast)=astreeCreate(AST_DIF, 0, (yyvsp[-2].ast), (yyvsp[0].ast), 0, 0, getLineNumber());}
#line 1848 "y.tab.c" /* yacc.c:1646  */
    break;


#line 1852 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 221 "parser.y" /* yacc.c:1906  */


int yyerror(char *msg){
    fprintf(stderr, "Syntax error at line %d! \n", getLineNumber());
    exit(3);
}

AST* getRoot(){
    return root;
}

