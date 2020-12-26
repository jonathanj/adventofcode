namespace AdventOfCode.Year2020
module Day21 =
  open AdventOfCode.Util

  let parse input =
    let parseOne = function
      | ParseRegex "^(.*?) \(contains (.*?)\)$" [String ingredients; String allergens] ->
          (Set (ingredients.Split(" ")), Set (allergens.Split(", ")))
      | _ -> failwith "Malformed input"
    Seq.map parseOne input

  let collateBy f g seq =
    Seq.fold
      (fun m v ->
        Map.change
          (f v)
          (fun o ->
            let v = g v
            match o with
            | Some xs -> Some (v::xs)
            | None -> Some [v])
          m)
      Map.empty
      seq

  let explodeRecipes (ingredients, allergens) =
    seq {
      for ingredient in ingredients do
        for allergen in allergens do
          (ingredient, allergen)
    }

  let findNonAllergenic recipes =
    let m =
      Seq.collect explodeRecipes recipes
      |> collateBy fst snd
      |> Map.map (fun _ xs -> Set xs)
    let allIngredients = Set (keys m)
    let removeAllergens m ingredient allergens =
      Map.change
        ingredient
        (fun o ->
          match o with
          | Some xs -> Some (Set.difference xs allergens)
          | _ -> failwith "Malformed state")
        m

    Seq.fold
      (fun result (ingredients, allergens) ->
        let otherIngredients = Set.difference allIngredients ingredients
        Seq.fold
          (fun r ingredient -> removeAllergens r ingredient allergens)
          result
          otherIngredients)
      m
      recipes
    |> Map.filter (fun _ v -> Set.isEmpty v)
    |> keys

  let foo recipes =
    let m =
      Seq.collect explodeRecipes recipes
      |> collateBy fst snd
      |> Map.map (fun _ xs -> Set xs)
    let allIngredients = Set (keys m)
    let removeAllergens m ingredient allergens =
      Map.change
        ingredient
        (fun o ->
          match o with
          | Some xs -> Some (Set.difference xs allergens)
          | _ -> failwith "Malformed state")
        m

    Seq.fold
      (fun result (ingredients, allergens) ->
        let otherIngredients = Set.difference allIngredients ingredients
        Seq.fold
          (fun r ingredient -> removeAllergens r ingredient allergens)
          result
          otherIngredients)
      m
      recipes
    // |> Map.filter (fun _ v -> Set.isEmpty v)
    // |> keys

  let sol1 recipes =
    let nonAllergenic = findNonAllergenic recipes
    Seq.sumBy
      (fun (ingredients, _) ->
        Set.intersect nonAllergenic ingredients
        |> Seq.length)
      recipes

  let sol2 recipes =
    let nonAllergenic = findNonAllergenic recipes
    let recipes =
      Seq.map
        (fun (ingredients, allergens) -> (Set.difference ingredients nonAllergenic, allergens))
        recipes

    let rec loop result recipes =
      match Map.filter (fun _ allergens -> Seq.length allergens = 1) (foo recipes) with
      | EmptyMap -> result
      | knowns -> 
        let ks = keys knowns
        let vs = Set.unionMany (vals knowns)
        loop
          (mergeMap result knowns)
          (Seq.map
            (fun (ingredients, allergens) -> (Set.difference ingredients ks, Set.difference allergens vs))
            recipes)

    loop Map.empty recipes
    |> Map.toList
    |> List.sortBy snd
    |> List.map fst
    |> String.concat ","

  let sample = parse [
    "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)";
    "trh fvjkl sbzzf mxmxvkd (contains dairy)";
    "sqjhc fvjkl (contains soy)";
    "sqjhc mxmxvkd sbzzf (contains fish)";
  ]

  let input = readLines (inputData 2020 21) |> parse

  let main () =
    printfn "%A" (sol1 input)
    printfn "%A" (sol2 input)
