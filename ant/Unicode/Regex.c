
#include <stdlib.h>
#include <string.h>


#ifdef CHAR_8BIT
  #define CHAR unsigned char
  #define EOF  0
  #define RUN_AUTOMATON  run_automaton_u8
  #define FREE_AUTOMATON free_automaton_u8
  #define COMPILE        compile_u8
#else
  #ifdef CHAR_16BIT
    #define CHAR unsigned short
    #define EOF  0
    #define RUN_AUTOMATON  run_automaton_u16
    #define FREE_AUTOMATON free_automaton_u16
    #define COMPILE        compile_u16
  #else
    #define CHAR unsigned int
    #define EOF  0xffffffff
    #define RUN_AUTOMATON  run_automaton_u32
    #define FREE_AUTOMATON free_automaton_u32
    #define COMPILE        compile_u32
  #endif
#endif

/* bitmaps */

typedef unsigned char *bitmap;

#define TEST_BIT(bm, i) ((bm)[(i) >> 3] &  (1 << ((i) & 0x7)))
#define SET_BIT(bm, i)  ((bm)[(i) >> 3] |= (1 << ((i) & 0x7)))

/* automata */

typedef struct
{
  const CHAR *label;
  int        next1;
  int        next2;
} state;

typedef struct
{
  int        num_states;
  int        final_state;
  state      *states;
  const CHAR *data;
  bitmap      bitmap1;
  bitmap      bitmap2;
} automaton;

/* there are three possible label formats:
 *
 *   NULL represents an epsilon transition
 *
 *   a character != '[' represents this character
 *
 *   '[' 0 n a_1 .. a_n represents "[^ a_1 .. a_n]"
 *
 *   '[' n a_1 .. a_n represents "[a_1 .. a_n]"
 */

#define EPSILON(automaton, state) ((automaton)->states[state].label == NULL)

static int bin_search(const CHAR *start, int n, CHAR c);

static int test_label(const CHAR *label, CHAR c)
{
  int n;

  if (label == NULL)
    return 1;

  if (*label != '[')
    return (c == *label);

  if ((n = label[1]) > 0)
    return bin_search(label + 1, n, c);
  else
    return !bin_search(label + 2, label[2], c);
};

static int bin_search(const CHAR *start, int n, CHAR c)
{
  const CHAR *l, *r, *m;

  /* the first element contains the number of characters to match */

  l = start;
  r = start + n - 1;

  /* binary search */

  while (l < r)
  {
    m = l + (r - l) / 2;

    if (*m < c)
      l = m + 1;
    else
      r = m;
  }

  return *l == c;
};

static void mark_states(const automaton *automaton, int state, bitmap bm)
{
tail_call:
  if (state < 0 || TEST_BIT(bm, state))
    return;

  SET_BIT(bm, state);

  if (EPSILON(automaton, state))
  {
    mark_states(automaton, automaton->states[state].next1, bm);

    /* tail call optimisation */

    state = automaton->states[state].next2;
    goto tail_call;
  };
};

static int step_automaton(const automaton *automaton, CHAR c, const bitmap old, bitmap new)
{
  int s, non_empty;

  memset(new, 0, (automaton->num_states + 7) / 8);

  non_empty = 0;

  for (s = 0; s < automaton->num_states; s++)
    if (TEST_BIT(old, s))
    {
      non_empty = 1;

      if (test_label(automaton->states[s].label, c))
        mark_states(automaton, automaton->states[s].next1, new);
      else
        mark_states(automaton, automaton->states[s].next2, new);
    }

  return non_empty;
};

int RUN_AUTOMATON(const automaton *automaton, CHAR (*get_char)(void *), void *get_char_data)
{
  bitmap bm1, bm2, b;
  CHAR   c;
  int    i, longest_match;

  bm1 = automaton->bitmap1;
  bm2 = automaton->bitmap2;

  memset(bm1, 0, (automaton->num_states + 7) / 8);

  SET_BIT(bm1, 0);      /* initial state */

  for (i = 0, longest_match = -1; (c = get_char(get_char_data)) != EOF; i++)
  {
    if (!step_automaton(automaton, c, bm1, bm2))
      break;                                            /* set of states is empty */

    if (TEST_BIT(bm2, automaton->final_state))
      longest_match = i;

    /* swap bitmaps */

    b = bm2; bm2 = bm1; bm1 = b;
  };

  return longest_match;
};

/* constructing automata */

void FREE_AUTOMATON(automaton *automaton)
{
  if (automaton)
  {
    free(automaton->states);
    free((void *)automaton->data);
    free(automaton->bitmap1);
    free(automaton->bitmap2);
    free(automaton);
  }
};

static CHAR *parse_regex(automaton *automaton, CHAR *regex, int state);

automaton *COMPILE(CHAR *regex)
{
  automaton *automaton;

  if ((automaton = malloc(sizeof(automaton))) == NULL)
    return NULL;

  automaton->data = regex;

  for (automaton->num_states = 0;
       regex[automaton->num_states] != EOF;
       automaton->num_states++)
    ;

  if ((automaton->states = malloc(automaton->num_states * sizeof(state))) == NULL)
  {
    FREE_AUTOMATON(automaton);
    return NULL;
  }

  regex = parse_regex(automaton, regex, 0);

  if (regex == NULL || *regex != EOF)
  {
    FREE_AUTOMATON(automaton);
    return NULL;
  }
  if ((automaton->bitmap1 = malloc(2 * (automaton->num_states+7)/8)) == NULL)
  {
    FREE_AUTOMATON(automaton);
    return NULL;
  }
  automaton->bitmap2 = automaton->bitmap1 + (automaton->num_states+7)/8;

  return automaton;
};

static state *add_state(automaton *automaton, int state, const CHAR *label, int next1, int next2)
{
  if (state >= automaton->num_states)
  {
    automaton->num_states *= 2;
    automaton->states = realloc(automaton->states, automaton->num_states * sizeof(state));
  };

  if (automaton->states)
  {
    automaton->states[state].label = label;
    automaton->states[state].next1 = next1;
    automaton->states[state].next2 = next2;
  }

  return automaton->states;
};

static int make_star(automaton *automaton, int first_state, int last_state)
{
  int s;

  /* shift states */

  for (s = first_state; s < last_state; s++)
  {
    if (automaton->states[s].next1 >= first_state)
      automaton->states[s].next1++;
    if (automaton->states[s].next2 >= first_state)
      automaton->states[s].next2++;

    automaton->states[s+1] = automaton->states[s];
  };

  /* insert new state before them */

  return add_state(automaton, last_state, NULL, last_state + 1, first_state + 1) == NULL;
};

static CHAR *parse_regex(automaton *automaton, CHAR *regex, int first_state)
{
  CHAR c, d;
  int  n, state;

tail_call:

  state = first_state;

  if (regex[0] == '[')
  {
    if (regex[1] == '^')
    {
      /* count number of characters and shift every char one position to the right */

      for (n = 3, c = regex[2]; regex[n] != ']' && regex[n] != EOF; n++)
      {
        d = regex[n]; regex[n] = c; c = d;
      };

      regex[n] = c;
      regex[1] = 0;
      regex[2] = n;
    }
    else
    {
      /* count number of characters and shift every char one position to the right */

      for (n = 2, c = regex[1]; regex[n] != ']' && regex[n] != EOF; n++)
      {
        d = regex[n]; regex[n] = c; c = d;
      };

      regex[n] = c;
      regex[1] = n;
    }

    if (add_state(automaton, state, regex, state + 1, -1) == NULL)
      return NULL;

    regex += n + 1;
    state++;
  }
  else if (regex[0] == '.')
  {
    if (add_state(automaton, state, NULL, state + 1, -1) == NULL)
      return NULL;

    regex++;
    state++;
  }
  else
  {
    if (add_state(automaton, state, regex, state + 1, -1) == NULL)
      return NULL;

    regex++;
    state++;
  }

  switch (regex[0])
  {
    case '*':
      if (!make_star(automaton, first_state, state))
        return NULL;

      regex++;
      state++;

      /* skip additional * and + */

      while (*regex == '*' || *regex == '+')
        regex++;

      break;

    case '+':
      if (add_state(automaton, state, NULL, state + 1, first_state) == NULL)
        return NULL;

      /* skip additional * and + */

      for (regex++; *regex == '*' || *regex == '+'; regex++)
      {
        if (*regex == '*')
        {
          /* convert the + into a * */

          if (!make_star(automaton, first_state, state))
            return NULL;
        };
      }

      state++;
      break;

    default:
      break;
  }

  first_state = state;
  goto tail_call;
};

