board <- board_ms365_test_charpath(delete_by_item = TRUE)
test_api_basic(board)
test_api_versioning(board)
test_api_meta(board)

board2 <- board_ms365_test_driveitem(delete_by_item = TRUE)
test_api_basic(board2)
test_api_versioning(board2)
test_api_meta(board2)
