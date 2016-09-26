(*
   Copyright 2008-2014 Nikhil Swamy and Microsoft Research

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

(* A logical theory of sequences indexed by natural numbers in [0, n) *)
module FStar.Seq
#set-options "--initial_fuel 0 --max_fuel 0 --initial_ifuel 1 --max_ifuel 1"

noeq abstract type seq (a:Type) =
| MkSeq: length:nat -> contents:(n:nat{n<length} -> Tot a) -> seq a

(* Destructors *)
abstract val length: #a:Type -> seq a -> Tot nat
let length #a s = MkSeq.length s

abstract val index:  #a:Type -> s:seq a -> i:nat{i < length s} -> Tot a
let index #a s i = MkSeq.contents s i

abstract val create: #a:Type -> nat -> a -> Tot (seq a)
let create #a len v =  MkSeq len (fun i -> v)

abstract val init: #a:Type -> len:nat -> contents: (i:nat { i < len } -> Tot a) -> Tot (seq a)
let init #a len contents = MkSeq len contents 

module L = FStar.List.Tot

abstract val of_list: #a:Type -> list a -> Tot (seq a)
let of_list #a l =
  MkSeq (L.length l) (L.index l)

abstract val length_of_list: #a:Type -> s:seq a -> l:list a -> Lemma
  (requires (s == of_list #a l))
  (ensures (length s = L.length l))
  [SMTPat (length s = L.length l)]
let length_of_list #a s l  = ()

abstract val index_of_list: #a:Type -> s:seq a -> l:list a -> i:nat{i < length s} -> Lemma
  (requires (s == of_list l))
  (ensures (s == of_list l /\ L.length l = length s /\ index s i == L.index l i))
  [SMTPat (index s i == L.index l i)]
let index_of_list #a s l i = ()

(* TR: should be renamed into something like lemma_lt_n_0_aux *)

private val exFalso0 : a:Type -> n:nat{n<0} -> Tot a
let exFalso0 a n = ()

(* CH: Seq.empty or emptySeq would be a better name for this? *)
(* TR: I'm in favor of [empty], cf. [FStar.Set.empty] *)
abstract val empty: #a:Type -> Tot (s:(seq a){length s=0})
let empty #a = MkSeq 0 (fun i -> (exFalso0 a i))

(* TR: upd -> [update]? *)
abstract val upd:    #a:Type -> s:seq a -> n:nat{n < length s} -> a ->  Tot (seq a)
let upd #a s n v =
  MkSeq (length s) (fun i -> if i=n then v else index s i)

abstract val append: #a:Type -> seq a -> seq a -> Tot (seq a)
let append #a s1 s2 =
  MkSeq (length s1 + length s2) (fun x -> if x < (length s1) then index s1 x else index s2 (x - (length s1)))
let op_At_Bar (#a:Type) (s1:seq a) (s2:seq a) = append s1 s2

abstract val slice:  #a:Type -> s:seq a -> i:nat -> j:nat{i <= j && j <= length s} -> Tot (seq a)
let slice #a s i j = 
  MkSeq (j-i) (fun x -> index s (x + i))

(* Lemmas about length *)

(* TR: len -> length *)
(* TR: create_length -> length_create *)

abstract val length_create: #a:Type -> n:nat -> i:a -> Lemma
  (requires True)
  (ensures (length (create n i) = n))
  [SMTPat (length (create n i))]
let length_create #a n i   = ()

abstract val length_init: #a:Type -> n:nat -> contents: (i:nat { i < n } -> Tot a) -> Lemma
  (requires True)
  (ensures (length (init n contents) = n))
  [SMTPat (length (create n contents))]
let length_init #a n contents = ()

(* TR: len -> length, upd_length -> length_upd *)

abstract val length_upd: #a:Type -> n:nat -> v:a -> s:seq a{n < length s} -> Lemma
  (requires True)
  (ensures (length (upd s n v) = length s))
  [SMTPat (length (upd s n v))]
let length_upd #a n v s    = ()

abstract val length_append: #a:Type -> s1:seq a -> s2:seq a -> Lemma
  (requires True)
  (ensures (length (append s1 s2) = length s1 + length s2))
  [SMTPat (length (append s1 s2))]
let length_append #a s1 s2 = ()

abstract val length_slice: #a:Type -> s:seq a -> i:nat -> j:nat{i <= j && j <= length s} -> Lemma
  (requires True)
  (ensures (length (slice s i j) = j - i))
  [SMTPat (length (slice s i j))]
let length_slice #a s i j  = ()

(* Lemmas about index *)
abstract val index_create: #a:Type -> n:nat -> v:a -> i:nat{i < n} -> Lemma
  (requires True)
  (ensures (index (create n v) i == v))
  [SMTPat (index (create n v) i)]
let index_create #a n v i  = ()

(* TR: Here we should avoid 1, 2, etc. as lemma names, in favor of more expressive suffixes
   example: for update-like features, same vs. other,
   for binary operations: left vs. right,
   etc. *)

abstract val index_upd_same: #a:Type -> s:seq a -> n:nat{n < length s} -> v:a -> Lemma
  (requires True)
  (ensures (index (upd s n v) n == v))
  [SMTPat (index (upd s n v) n)]
let index_upd_same #a n v s    = ()

abstract val index_upd_other: #a:Type -> s:seq a -> n:nat{n < length s} -> v:a -> i:nat{i<>n /\ i < length s} -> Lemma
  (requires True)
  (ensures (index (upd s n v) i == index s i))
  [SMTPat (index (upd s n v) i)]
let index_upd_other #a n v s i  = ()

(* TR: question: _left -> _l? *)

abstract val index_append_left: #a:Type -> s1:seq a -> s2:seq a -> i:nat{i < length s1} -> Lemma
  (requires True)
  (ensures (index (append s1 s2) i == index s1 i))
  [SMTPat (index (append s1 s2) i)]
let index_append_left #a s1 s2 i  = ()

(* TR: question: _right -> _r? *)

abstract val index_append_right: #a:Type -> s1:seq a -> s2:seq a -> i:nat{i < length s1 + length s2 /\ length s1 <= i} -> Lemma
  (requires True)
  (ensures (index (append s1 s2) i == index s2 (i - length s1)))
  [SMTPat (index (append s1 s2) i)]
let index_append_right #a s2 s2 i  = ()

abstract val index_slice: #a:Type -> s:seq a -> i:nat -> j:nat{i <= j /\ j <= length s} -> k:nat{k < j - i} -> Lemma
  (requires True)
  (ensures (index (slice s i j) k == index s (k + i)))
  [SMTPat (index (slice s i j) k)]
let index_slice #a s i j k = ()

abstract type equal (#a:Type) (s1:seq a) (s2:seq a) =
  (length s1 = length s2
   /\ (forall (i:nat{i < length s1}).{:pattern (index s1 i); (index s2 i)} (index s1 i == index s2 i)))

(* decidable equality *)
private val eq_i:
  #a:eqtype -> s1:seq a -> s2:seq a{length s1 = length s2}
  -> i:nat{i <= length s1}
  -> Tot (r:bool{r <==> (forall j. (j >= i /\ j < length s1) ==> (index s1 j = index s2 j))})
    (decreases (length s1 - i))
let rec eq_i #a s1 s2 i =
  if i = length s1 then true
  else
    if index s1 i = index s2 i then eq_i s1 s2 (i + 1)
    else false

abstract val eq: #a:eqtype -> s1:seq a -> s2:seq a -> Tot (r:bool{r <==> equal s1 s2})
let eq #a s1 s2 = if length s1 = length s2 then eq_i s1 s2 0 else false

abstract val eq_intro: #a:Type -> s1:seq a -> s2:seq a -> Lemma
     (requires (length s1 = length s2
               /\ (forall (i:nat{i < length s1}).{:pattern (index s1 i); (index s2 i)} (index s1 i == index s2 i))))
     (ensures (equal s1 s2))
     [SMTPatT (equal s1 s2)]
let eq_intro #a s1 s2 = ()

abstract val lemma_eq_refl: #a:Type -> s1:seq a -> s2:seq a -> Lemma
     (requires (s1 == s2))
     (ensures (equal s1 s2))
     [SMTPatT (equal s1 s2)]
let lemma_eq_refl #a s1 s2  = ()

(*TODO: Would be nice to to not have to assume this again and instead derive it from feq
  But, it doesn't work because in order to use feq, we need to show that s1.contents has type (efun e b) *)
assume Extensionality: forall (a:Type) (s1:seq a) (s2:seq a).{:pattern (equal s1 s2)} equal s1 s2 <==> (s1==s2)
abstract val eq_elim: #a:Type -> s1:seq a -> s2:seq a -> Lemma
     (requires (equal s1 s2))
     (ensures (s1==s2))
     [SMTPatT (equal s1 s2)]
let eq_elim #a s1 s2  = ()
