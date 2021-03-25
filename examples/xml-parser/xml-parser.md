# xml-parser

The `xml-parser` executables are made from this source:

```
// xml-parser.c
#include <xml/xml.h>
#include <xml/print.h>

int main(int argc, const char * argv[]) {
  xml_doc_t *doc;
  xml_t     *root;

  if (argc < 2) {
    printf("expected an XML string argument\n");
    return 1;
  }
  
  doc  = xml_parse(argv[1], XML_DEFAULTS);
  root = doc->root;

  xml_print_human(stderr, root);

  xml_free(doc);

  return 0;
}
```

and the C XML library [found here](https://github.com/recp/xml) (commit 7bbe11691ce8143483ec0164d008e30c6e718e8d).


E.g., `diet clang -static -g xml-parser.c -o xml-parser-diet-clang`.

