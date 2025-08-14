# REST-API

This is the CopyCommander2 Rest API Documentation.

CopyCommander REST API only supports JSON as data format. To see how to enable / setup the REST functionality visit the [rest api](how_to_use.md#rest-api) section in the how_to_use document.

There are 2 kinds of paths supported by CopyCommander2

#### Normal paths:
Normal paths are always available and can be used to get status, start jobs, ...

#### Zombie paths:
Zombie paths are used to control the application (visible to the user), to enable these paths you need to pass in a extra command line paramter.

| Method | Overview | Kind | Description
| --- | --- | --- | --- |
| GET | [/API/status](#get-apistatus) | normal | get current state of application
| POST | [/API/zombie/setdir](#post-apizombiesetdir) | zombie | set directory path for left or right view

## GET /API/status

Returns the current status of the application.

### Request

**Method:** GET  
**Path:** `/API/status`  
**Content-Type:** Not applicable

### Response

**Content-Type:** `application/json`

### Response Schema

| Field | Type | Description |
|-------|------|-------------|
| `State` | integer | Current application state (0 = idle, 1 = busy) |
| `JobQueueCount` | integer | Number of jobs currently in the queue |
| `LeftDir` | string | Path to the left directory |
| `RightDir` | string | Path to the right directory |

### Example Response

```json
{
    "State": 0,
    "JobQueueCount": 3,
    "LeftDir": "C:\\Users\\john\\Documents\\Source",
    "RightDir": "C:\\Users\\john\\Documents\\Backup"
}
```
## GET /API/view/list

Returns the list of files and folders in the specified view.

### Request

**Method:** GET  
**Path:** `/API/view/list`  
**Content-Type:** Not applicable

### Query Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `view` | string | No | View to list ("left" or "right"). Defaults to "left" if not specified |

### Response

**Content-Type:** `application/json`

### Response Schema

| Field | Type | Description |
|-------|------|-------------|
| `dir` | string | Absolute path of the current directory |
| `folders` | array | Array of folder names in the view |
| `files` | array | Array of file names in the view |

### Example Request

```
GET /API/view/list?view=right
```

### Example Response

```json
{
    "folders": [
        "Documents",
        "Pictures",
        "Videos"
    ],
    "files": [
        "readme.txt",
        "config.json",
        "backup.zip"
    ]
}
```

## POST /API/zombie/setdir

Sets the directory path for either the left or right view.

### Request

**Method:** POST  
**Path:** `/API/zombie/setdir`  
**Content-Type:** `application/json`

### Request Schema

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `view` | string | Yes | Must be either "left" or "right" |
| `dir` | string | Yes | Directory path to set |

### Example Request

```json
{
    "view": "left",
    "dir": "C:\\Users\\john\\Documents\\NewSource"
}
```

### Response

**Status Codes:**
- `204 No Content` - Directory successfully set
- `422 Unprocessable Entity` - Invalid request data / target dir, does not exist.