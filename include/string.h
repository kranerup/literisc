/* string.h - minimal liteRISC shim
 *
 * Only memcpy/memset are declared, since that's all liteRISC-targeted
 * code has needed so far. Add implementations here if a program actually
 * calls them.
 */

#ifndef STRING_H
#define STRING_H

extern void *memcpy(void *dest, void *src, int n);
extern void *memset(void *dest, int c, int n);

#endif /* STRING_H */
