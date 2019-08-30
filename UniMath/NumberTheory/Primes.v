(** * Results about Number Theory *)
(** Author: Junstin Scarfy. Jun 2019 - *)
(** Based on Bourbaky *)

Require Import UniMath.Foundations.All.
Require Import UniMath.Foundations.MoreFoundations.

(**  natural numbers *)
Definition nat_mult : nat → nat → nat :=
  nat_rect (λ _, nat → nat) (λ _, 0) (λ _ IHm n, nat_plus n (IHm n)).


(**  prime numbers *)
Definition is_prime : nat → UU :=
  λ n, ∑ _:((n = 1) → empty), (∏ m1 m2, nat_mult m1 m2 = n → (m1 = 1) ⨿ (m2 = 1)).
  

(**  infinitute of prime numbers *)
