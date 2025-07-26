# parse_biocheck_json handles missing file

    Code
      parse_biocheck_json("nonexistent_file.json")
    Condition
      Error:
      ! lexical error: invalid string in json text.
                                             nonexistent_file.json
                           (right here) ------^

# parse_biocheck_json handles malformed JSON

    Code
      parse_biocheck_json(temp_json)
    Condition
      Error in `parse_con()`:
      ! lexical error: invalid char in json text.
                tus":"Stabilizing","Ticks":[{malformed}]}  
                           (right here) ------^

