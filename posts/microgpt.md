---
title: Rewriting MicroGPT in Javascript
author: Arnaud Bailly
date: 2026-04-05
---

I have been dabbling in ML every now and then, implementing a [Haskell version of Word2Vec](https://github.com/abailly/hs-word2vec), attending [ML Week](https://pankzsoft.leaflet.pub/3makxlccbyi66) ten (!) years ago to improve my understanding of basic techniques and tools, and occasionaly reading articles and posts. Since I have started using AI agents for coding on a regular basis, witnessing how effective it, I have become more and more worried about the capture of knowledge and value centralised and monopolistic tools like ChatGPT or Claude represent.

I am trying with [other](https://app.radicle.xyz/nodes/seed.hydra.bzh/rad:z2o7yJKSodNLjtqDfna8LPDhx4Shg) like-minded people to explore options allowing us to regain some control over those tools, e.g. local hosting or co-hosting models, and it seemed important in that context to beef up my knowledge and understanding of how Generative AI actually works. Andrej Karpathy's [microgpt blog post](https://karpathy.github.io/2026/02/12/microgpt/) is fascinating as it somehow synthesizes decades of experiments and scientific breakthrough in about 200 lines of Python code[^1] and being the coder I am, it seems there's no better way for me to understand something than to build or rebuild it.

At [SoCraTes Rennes](https://pankzsoft.leaflet.pub/3mihkn6degk64), "Gul" and I spent an hour or so trying to hack a Javascript implementation. We did not go very far but I continued what we started and it took me less than a week to build a working version that does the same thing as the Python version: Training a model on a set of first names and infer new first names. The net result is available [here](https://app.radicle.xyz/nodes/seed.hydra.bzh/rad:z2TVj3gRjRcaVGu3Rxf6viRDwzvQn).

I obviously did not use a LLM to write (most of) the code which would have defeated the purpose of understanding how to write code implementing the needed features to build a neural network based transformers and multi-head attention, and I wrote one or a couple of tests before implementing every major step. Karpathy's post is very clear and detailed and implemeting the various parts was quite straightforward even without understand each step's details.

However, it's still quite dense and after implementing the the whole process, stitching everything together into a training loop, I found out my code was buggy: After less than a handful of training iterations, `NaN` values started to propagate across the network quickly and made it unusable. I spent some time scratching my head, using `console.log` to debug the problem, trying to located where those `NaN` values were produced. After a while I fired up [pi.dev](https://pi.dev) and asked Claude to help me pinpoint the issues, which it did in a couple of minutes giving me a list of no less than 4 bugs which I reproduce verbatim here.

```
 ┌───┬────────────┬─────────────────────────────────────────────────────────────────────────────────────────────┬───────────────────────────────────────────┐
 │ # │ Location   │ Bug                                                                                         │ Fix                                       │
 ├───┼────────────┼─────────────────────────────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────┤
 │ 1 │ div()      │ Local gradients were [b, a] (copied from mul) instead of [1/b, -a/b²], causing every        │ Corrected to [1 / operand.data,           │
 │   │            │ backward pass through division to propagate completely wrong gradients — the direct cause   │ -this.data / (operand.data ** 2)]         │
 │   │            │ of training blowing up to Infinity                                                          │                                           │
 ├───┼────────────┼─────────────────────────────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────┤
 │ 2 │ linear()   │ Loops iterated i over input length and j over w.cols, so the output had the wrong size and  │ Fixed loop to iterate i over w.rows and j │
 │   │            │ computed the wrong dot products. As a side-effect mlp_fc1/mlp_fc2 matrix dimensions were    │  over input length; swapped               │
 │   │            │ also swapped in StateDict, which had been accidentally masked by the broken loop            │ mlp_fc1/mlp_fc2 dimensions                │
 ├───┼────────────┼─────────────────────────────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────┤
 │ 3 │ cache()    │ Used keys.length (number of layers) as the splice index instead of keys[li].length (entries │ Changed to keys[li].push(k) /             │
 │   │            │ in that layer's cache), so new tokens were never appended and attention always saw only the │ values[li].push(v)                        │
 │   │            │ first token                                                                                 │                                           │
 ├───┼────────────┼─────────────────────────────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────┤
 │ 4 │ gpt()      │ Passed the raw embedding x to attention() as the query instead of the projected query q =   │ Made cache() return q and passed it       |
 │   │            │ linear(x, attn_wq). kv_cache() computed q internally but discarded it, making the query     │ into attention() in gpt()                 │
 │   │            │ weight matrix attn_wq completely unused during training                                     │                                           │
 └───┴────────────┴─────────────────────────────────────────────────────────────────────────────────────────────┴───────────────────────────────────────────┘
```

What's interesting, and a good reminder that speed should be evaluated over the whole process, is that the first 2 bugs at least could have been caught by more careful testing. The `Value` class which encapsulates model's parameters along with propagation links and gradients, is not complicated but of course needs to be 100% correct as it's used everywhere in the codebase. Javascript does not provide operators' overloading like Python does so arithmetic expressions cannot be used directly which begs for a lot more tests than I wrote to guarantee correctness. I built up the `Value` class incrementaly, adding operations as they were needed for specific parts of the model training, and did not always add the individual tests. In particular I did not always check the gradients were always computed correctly and this is what happened for the `div` operation: It appeared somewhat later in the process and I missed the gradient computation part.

The second issue whereby I inverted vector/matrix multiplication logic, is even more a blatant case of missing tests: I wrote a single test with a square matrix! This issue should have surfaced earlier hadn't I inadvertently swapped the dimensions of fc1/fc2 matrices.

The third issue was actually an hallucination and when Claude wrote the test it passed immediately: The `keys` and `values` caches were already correctly populated! It's possible it was tripped by the fact the fourth issue is linked to the same location in the code as the `q` project query is computed in the `kv_cache` method, and the fix required to return this `q`. It turns out there was another issue, which Claude did not spot, in the `gpt` method: not only was `x` (the embedding vector of the current token) was used in place of `q`, but it was also not normalised before hitting the cache .

```
--- a/microgpt.js
+++ b/microgpt.js
@@ -214,6 +214,7 @@ export class StateDict {
     const v = linear(embedding, this[`layer_${li}`].attn_wv);
     keys[li].splice(keys.length, 0, k);
     values[li].splice(values.length, 0, v);
+    return q;
   }

   // Compute attention for single head in a single layer using given keys and values cache
@@ -294,10 +295,11 @@ export class StateDict {
     let x = this.embedding(token_id, pos_id);
     for (let li = 0; li < this.n_layer; li++) {
       let x_residual = x;
-      this.kv_cache(li, x, keys, values);
+      x = rmsnorm(x);
+      let q = this.kv_cache(li, x, keys, values);
       const x_attn = [];
       for (let h = 0; h < this.n_head; h++) {
-        x_attn.push(...this.attention(li, h, x, keys, values));
+        x_attn.push(...this.attention(li, h, q, keys, values));
       }
       [x_residual, x] = this.mlp(li, x_attn, x_residual, x);
     }
```

Running the training then inference engine gives the following output, for a temperature of 0.4:

```
% node microgpt.js
Step: 1000 Loss:2.54
Generated Samples:
  aakni
  noaa
  azia
  amain
  daael
  azoin
  alanen
  samari
  naeel
  eamen
  arooa
  asan
  nanin
  alaei
  salon
  ieaa
  ansa
  aana
  elaira
  nazan
```

to compare with `microgpt.py`'s output:

```
python3 microgpt.py
num docs: 32033
vocab size: 27
num params: 4192
step 1000 / 1000 | loss 2.5454
--- inference (new, hallucinated names) ---
sample  1: jann
sample  2: jari
sample  3: jaran
sample  4: dalie
sample  5: salela
sample  6: jaria
sample  7: shele
sample  8: anilyn
sample  9: aniri
sample 10: ana
sample 11: jonia
sample 12: jaylen
sample 13: karan
sample 14: alerin
sample 15: abran
sample 16: kanle
sample 17: jana
sample 18: kara
sample 19: alela
sample 20: anora
```

Anecdotally, the Javascript code appears roughly 2-3x faster than Python on my machine.

I now plan to further study that code and refine my understanding of the various techniques involved, and possibly implement it in other languages I am more proficient in than Javascript like Haskell or Common Lisp. I even consider writing an implementation in Forth for the sheer joy of banging my head against my desk :)

[^1]: This is quite an emphatic statement, but nevertheless not too far from the truth as Chat GPT 2 was the first publicly available version that triggered the current craze by enrolling millions in a matter of weeks or months.
