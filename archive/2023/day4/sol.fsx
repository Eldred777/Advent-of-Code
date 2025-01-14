type LottoNumbers =
    { Id: int
      WinningNumbers: list<int>
      ChosenNumbers: list<int> }

let score x = if x = 0 then 0 else pown 2 x
|>