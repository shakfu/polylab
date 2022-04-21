creates a python extension from a nim file


```bash
nim c --app:lib --out:add.so --threads:on add
```

then

```python
import add
add.add(10, 20)
30
```
