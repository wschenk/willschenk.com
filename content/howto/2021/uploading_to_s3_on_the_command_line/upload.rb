require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'aws-sdk-s3'
  gem 'rexml'
end

def upload_object(s3_client, bucket_name, file_name)
  response = s3_client.put_object(
    body: File.read(file_name),
    bucket: bucket_name,
    key: file_name,
    acl: 'public-read'
  )
  if response.etag
    return true
  else
    return false
  end
rescue StandardError => e
  puts "Error uploading object: #{e.message}"
  return false
end

def run_me
  ['AWS_ACCESS_KEY_ID', 'AWS_SECRET_ACCESS_KEY', 'AWS_END_POINT', 'BUCKET_NAME'].each do |key|
    throw "Set #{key}" if ENV[key].nil?
  end

  region = 'us-east-1'
  s3_client = Aws::S3::Client.new(access_key_id: ENV['AWS_ACCESS_KEY_ID'],
                                  secret_access_key: ENV['AWS_SECRET_ACCESS_KEY'],
                                  endpoint: ENV['AWS_END_POINT'],
                                  region: region)
  uploaded = upload_object(s3_client, ENV['BUCKET_NAME'], 'upload.rb')
  puts "Uploaded = #{uploaded}"
end

run_me if $PROGRAM_NAME == __FILE__
