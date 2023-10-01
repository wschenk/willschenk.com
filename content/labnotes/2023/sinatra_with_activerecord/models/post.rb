# models/post.rb
class Post < ActiveRecord::Base
  validates_presence_of :name, :body
end
